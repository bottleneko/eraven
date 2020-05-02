-module(er_logger_handler).

-define(MICROSECONDS_IN_SECONDS, 1000000).

-define(UNDEFINED_FUNCTION_REPLACEMENT, <<"Undefined report function">>).
-define(WRONG_ARITY_REPLACEMENT, <<"Wrong report function arity">>).

% Logger callbacks
-export([log/2]).             -ignore_xref([log/2]).
-export([adding_handler/1]).  -ignore_xref([adding_handler/1]).
-export([changing_config/3]). -ignore_xref([changing_config/3]).

%%%===================================================================
%%% Logger callbacks
%%%===================================================================

-spec log(LogEvent, HandlerConfig) -> any() when
    LogEvent      :: logger:log_event(),
    HandlerConfig :: logger:handler_config().
log(#{msg   := Message,
      level := Level,
      meta  := Meta} = _LogEvent,
    #{config := #{dsn                  := Dsn,
                  json_encode_function := JsonEncodeFunction,
                  event_tags_key       := EventTagsKey,
                  event_extra_key      := EventExtraKey,
                  fingerprint_key      := FingerprintKey
                 } = Config
     } = _HandlerConfig) ->
  EnvironmentContext = maps:get(environment_context, Config),

  RequestContext = maps:get(eraven_request_context, Meta, undefined),

  UserContext = maps:get(eraven_user_context, Meta, undefined),

  ProcessTags = maps:get(eraven_process_tags, Meta, #{}),
  EventTags = maps:get(EventTagsKey, Meta, #{}),
  Tags = maps:merge(ProcessTags, EventTags),

  ProcessExtra = maps:get(eraven_process_extra, Meta, #{}),
  EventExtra = maps:get(EventExtraKey, Meta, #{}),
  Extra = maps:merge(ProcessExtra, EventExtra),

  Fingerprint = maps:get(FingerprintKey, Meta, [<<"{{ default }}">>]),

  Context = er_context:new(EnvironmentContext,
                           RequestContext,
                           Extra,
                           UserContext,
                           Tags,
                           _Breadcrumbs = #{},
                           Fingerprint),

  Event = build_event(format_message(Message, Meta, Config), Level, Meta, Context),
  er_client:send_event(Event, Dsn, JsonEncodeFunction);
log(LogEvent, HandlerConfig) ->
  io:format("Eraven log function clause.~n"
            "LogEvent: ~p~n"
            "HandlerConfig: ~p~n", [LogEvent, HandlerConfig]).

-spec adding_handler(HandlerConfig) -> {ok, NewHandlerConfig} | {error, Reason} when
    HandlerConfig    :: logger:handler_config(),
    NewHandlerConfig :: logger:handler_config(),
    Reason           :: binary().
adding_handler(HandlerConfig) ->
  RawConfig = maps:get(config, HandlerConfig, #{}),
  case er_config:new(RawConfig) of
    {ok, Config} ->
      {ok, HandlerConfig#{config => Config}};
    {error, _Reason} = Error ->
      Error
  end.

-spec changing_config(SetOrUpdate, OldHandlerConfig, NewHandlerConfig) -> Result when
    SetOrUpdate      :: set | update,
    OldHandlerConfig :: logger:handler_config(),
    NewHandlerConfig :: logger:handler_config(),
    Result           :: {ok, Config}
                      | {error, Reason},
    Config           :: logger:handler_config(),
    Reason           :: binary().
changing_config(set, OldHandlerConfig, NewHandlerConfig) ->
  changing_config(update, OldHandlerConfig#{config => #{}}, NewHandlerConfig);
changing_config(update, OldHandlerConfig, NewHandlerConfig) ->
  OldRawConfig = maps:get(config, OldHandlerConfig, #{}),
  NewRawConfig = maps:get(config, NewHandlerConfig, #{}),
  RawConfig = maps:merge(OldRawConfig, NewRawConfig),
  case er_config:new(RawConfig) of
    {ok, Config} ->
      {ok, NewHandlerConfig#{config => Config}};
    {error, _Reason} = Error ->
      Error
  end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec format_message(Message, Meta, Config) -> binary() when
    Message :: {string, unicode:chardata()}
             | {report, logger:report()}
             | {io:format(), [term()]},
    Meta    :: logger:metadata(),
    Config  :: er_config:t().
format_message({string, String}, _Meta, _Config) ->
  String;
format_message({report, Report}, Meta, Config) ->
  format_report(Report, Meta, Config);
format_message({Format, Data}, _Meta, _Config) ->
  Formatted = io_lib:format(Format, Data),
  iolist_to_binary(Formatted).

-spec format_report(Report, Meta, Config) -> binary() when
    Report :: logger:report(),
    Meta   :: logger:metadata(),
    Config :: er_config:t().
format_report(Report, Meta, #{report_depth := Depth, report_chars_limit := Limit} = _Config) ->
  Fun = fun logger:format_otp_report/1,
  case maps:get(report_cb, Meta, undefined) of
    undefined ->
      ?UNDEFINED_FUNCTION_REPLACEMENT;
    Fun ->
      case Report of
        #{report := _, label := _} ->
          {Format, Arguments} = Fun(Report),
          unicode:characters_to_binary(io_lib:format(Format, Arguments));
        _ ->
          unicode:characters_to_binary(io_lib:format("~p", [Report]))
      end;
    ReportFun when is_function(ReportFun, 1) ->
      {Format, Arguments} = ReportFun(Report),
      unicode:characters_to_binary(io_lib:format(Format, Arguments));
    ReportFun when is_function(ReportFun, 2) ->
      ReportFun(Report, #{depth       => Depth,
                          chars_limit => Limit,
                          single_line => false
                         });
    _ReportFun ->
      ?WRONG_ARITY_REPLACEMENT
  end.

-spec build_event(Message, Level, Metadata, Context) -> er_event:t() when
    Message  :: binary(),
    Level    :: logger:level(),
    Metadata :: logger:metadata(),
    Context  :: er_context:t().
build_event(Message,
            Level,
            #{type       := Type,
              reason     := Reason,
              stacktrace := Stacktrace,
              time       := TimestampUs} = _Meta,
            Context) ->
  Timestamp = TimestampUs div ?MICROSECONDS_IN_SECONDS,
  er_event:new(Message, Level, Type, Reason, Stacktrace, Context, Timestamp);
build_event(Message,
            Level,
            #{mfa  := {Module, _Function, _Arity},
              line := Line,
              time := TimestampUs} = _Meta,
            Context) ->
  Timestamp = TimestampUs div ?MICROSECONDS_IN_SECONDS,
  er_event:new(Message, Level, Module, Line, Context, Timestamp);
build_event(Message,
            Level,
            #{time := TimestampUs} = _Meta,
            Context) ->
  Timestamp = TimestampUs div ?MICROSECONDS_IN_SECONDS,
  er_event:new(Message, Level, Context, Timestamp).
