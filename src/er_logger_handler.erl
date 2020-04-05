-module(er_logger_handler).

-define(MICROSECONDS_IN_SECONDS, 1000000).

% Logger callbacks
-export([log/2, adding_handler/1, changing_config/3]).

% service
-export([default_config/0]).

-define(UNDEFINED_FUNCTION_REPLACEMENT, <<"Undefined report function">>).
-define(WRONG_ARITY_REPLACEMENT, <<"Wrong report function arity">>).

-define(DEFAULT_CONFIG, #{
  event_extra_key => event_extra,
  event_tags_key => event_tags,
  fingerprint_key => fingerprint,
  json_encode_function => fun jsx:encode/1,
  report_depth => 20,
  report_chars_limit => 4096
}).

%%%===================================================================
%%% Logger callbacks
%%%===================================================================

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
  EnvironmentContext = maps:get(environment_context, Config, undefined),

  RequestContext = maps:get(eraven_request_context, Meta, undefined),

  UserContext = maps:get(eraven_user_context, Meta, undefined),

  ProcessTags = maps:get(eraven_process_tags, Meta, #{}),
  EventTags = maps:get(EventTagsKey, Meta, #{}),
  Tags = maps:merge(ProcessTags, EventTags),

  ProcessExtra = maps:get(eraven_process_extra, Meta, #{}),
  EventExtra = maps:get(EventExtraKey, Meta, #{}),
  Extra = maps:merge(ProcessExtra, EventExtra),

  Fingerprint = maps:get(FingerprintKey, Meta, [<<"{{ default }}">>]),

  Context = er_context:new(EnvironmentContext, RequestContext, Extra, UserContext, Tags, #{}, Fingerprint),
  Event = build_event(format_message(Message, Meta, Config), Level, Meta, Context),
  er_client:send_event(Event, Dsn, JsonEncodeFunction);
log(LogEvent, HandlerConfig) ->
  io:format("Eraven log function clause.~nLogEvent: ~p~nHandlerConfig: ~p~n", [LogEvent, HandlerConfig]).

adding_handler(Config) ->
  Config2 = Config#{config => maps:merge(?DEFAULT_CONFIG, maps:get(config, Config, #{}))},
  parse_dsn(Config2).

changing_config(set, _OldConfig, NewConfig) ->
  changing_config(update, ?DEFAULT_CONFIG, NewConfig);
changing_config(update, OldConfig, NewConfig) ->
  OldParams = maps:get(config, OldConfig, #{}),
  NewParams = maps:get(config, NewConfig, #{}),
  Config = NewConfig#{config => maps:merge(OldParams, NewParams)},
  parse_dsn(Config).

%%%===================================================================
%%% API
%%%===================================================================

-spec default_config() -> map().
default_config() ->
  ?DEFAULT_CONFIG.

%%%===================================================================
%%% Internal functions
%%%===================================================================

format_message({string, Message}, _Meta, _Config) ->
  Message;
format_message({report, Report}, Meta, Config) ->
  format_report(Report, Meta, Config);
format_message({Format, Data}, _Meta, _Config) ->
  Formatted = io_lib:format(Format, Data),
  iolist_to_binary(Formatted).

-spec format_report(Report :: map(), Meta :: map(), Config :: map()) -> binary().
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
      ReportFun(Report, #{
        depth => Depth,
        chars_limit => Limit,
        single_line => false}
      );
    _ReportFun ->
      ?WRONG_ARITY_REPLACEMENT
  end.

-spec parse_dsn(Config :: map()) -> {ok, map()} | {error, binary()}.
parse_dsn(#{config := #{dsn := DsnString}} = Config) ->
  #{config := ConfigL2} = Config,
  case er_dsn:new(DsnString) of
    {ok, Dsn} ->
      {ok, Config#{config => ConfigL2#{dsn => Dsn}}};
    Reason ->
      {error, Reason}
  end;
parse_dsn(#{config := _ConfigL2}) ->
  Res = {error, <<"Missing required property `dsn`">>},
  io:format("~p:~p ~p~n", [?MODULE, ?LINE, Res]),
  Res;
parse_dsn(Config) -> % no config at all - maybe will be set during runtime
  {ok, Config}.

-spec build_event(Message, Level, Metadata, Context) -> er_event:t() when
    Message  :: binary(),
    Level    :: logger:level(),
    Metadata :: map(),
    Context  :: er_context:t().
build_event(Message, Level, #{type := Type, reason := Reason, stacktrace := Stacktrace, time := Timestamp} = _Meta, Context) ->
  er_event:new(Message, Level, Type, Reason, Stacktrace, Context, Timestamp div ?MICROSECONDS_IN_SECONDS);
build_event(Message, Level, #{mfa := {Module, _Function, _Arity}, line := Line, time := Timestamp} = _Meta, Context) ->
  er_event:new(Message, Level, Module, Line, Context, Timestamp div ?MICROSECONDS_IN_SECONDS);
build_event(Message, Level, #{time := Timestamp} = _Meta, Context) ->
  er_event:new(Message, Level, Context, Timestamp div ?MICROSECONDS_IN_SECONDS).
