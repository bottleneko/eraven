-module(er_event).

-record(er_event, {timestamp               :: non_neg_integer(),
                   message                 :: binary(),
                   level                   :: fatal | error | warning | info | debug,
                   platform = <<"erlang">> :: binary(),
                   exception               :: #{type => error | throw, value => Reason :: term()},
                   stacktrace              :: term() | undefined,
                   context                 :: term(),
                   module                  :: atom() | undefined,
                   line                    :: non_neg_integer() | undefined
%                   modules             :: [atom()]
                  }).

-opaque t() :: #er_event{}.

-export_type([t/0]).

-export([new/4, new/6, new/7]).
-export([to_map/1]).

%%%===================================================================
%%% API
%%%===================================================================

new(Message, Level, Context, Timestamp) ->
  #er_event{
     timestamp = Timestamp,
     message   = to_binary(Message),
     level     = map_event_level(Level),
     context   = Context
   }.

new(Message, Level, Module, Line, Context, Timestamp) ->
  #er_event{
     timestamp = Timestamp,
     message   = to_binary(Message),
     level     = map_event_level(Level),
     module    = Module,
     line      = Line,
     context   = Context
   }.

new(Message, Level, Type, Reason, Stacktrace, Context, Timestamp) ->
  #er_event{
     timestamp  = Timestamp,
     message    = to_binary(Message),
     level      = map_event_level(Level),
     exception  = #{type => Type, value => iolist_to_binary(io_lib:print(Reason))},
     stacktrace = Stacktrace,
     context    = Context
    }.

to_map(Event) ->
  Context = Event#er_event.context,
  EnvironmentContext = er_context:environment_context(Context),

  OptionalData = format_optional_data(Event),

  OptionalData#{
    timestamp   => Event#er_event.timestamp,
    message     => Event#er_event.message,
    level       => Event#er_event.level,
    platform    => Event#er_event.platform,
    server_name => er_environment_context:server_name(EnvironmentContext),
    environment => er_environment_context:environment(EnvironmentContext),
    release     => er_environment_context:release(EnvironmentContext),
    request     => er_context:request_context(Context),
    extra       => er_context:extra(Context),
    user        => er_context:user(Context),
    tags        => er_context:tags(Context),
    breadcrumbs => er_context:breadcrumbs(Context),
    fingerprint => er_context:fingerprint(Context)
   }.

%%%===================================================================
%%% Sentry formatters
%%%===================================================================

format_optional_data(Event) ->
  MaybeDataFromStacktrace = maybe_data_from_stacktrace(Event),
  MaybeCulpritFromLocation = maybe_culprit_from_location(Event),
  maps:merge(MaybeDataFromStacktrace, MaybeCulpritFromLocation).

maybe_data_from_stacktrace(#er_event{stacktrace = Stacktrace} = _Event) when Stacktrace =:= undefined; Stacktrace =:= [] ->
  #{};
maybe_data_from_stacktrace(#er_event{stacktrace = [StacktraceLine | _RestStacktrace] = Stacktrace} = Event) ->
  {Module, Function, ArgsOrArity, _Location} = StacktraceLine,
  Culprit = format_mfa(Module, Function, arity_to_integer(ArgsOrArity)),
  #{culprit     => Culprit,
    exception   => Event#er_event.exception,
    stacktrace  => format_stacktrace(Stacktrace)
   }.

maybe_culprit_from_location(#er_event{module = Module, line = Line} = _Event) when Module =:= undefined; Line =:= undefined ->
  #{};
maybe_culprit_from_location(#er_event{module = Module, line = Line} = _Event) ->
  Formatted = io_lib:format("~p:~B", [Module, Line]),
  #{culprit => iolist_to_binary(Formatted)}.

format_stacktrace(Stacktrace) ->
  Map =
    fun({Module, Function, ArgsOrArity, Location}) ->
        Arity = arity_to_integer(ArgsOrArity),

        #{filename => to_binary(proplists:get_value(file, Location)),
          function => format_mfa(Module, Function, Arity),
          module   => Module,
          lineno   => proplists:get_value(line, Location),
          vars     => parse_args(ArgsOrArity)
         }
    end,
  #{frames => lists:map(Map, Stacktrace)}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% https://tools.ietf.org/html/rfc5424
-spec map_event_level(SyslogEventLevel) -> SentryEventLevel when
    SyslogEventLevel :: atom(),
    SentryEventLevel :: atom().
map_event_level(emergency) -> fatal;
map_event_level(alert)     -> fatal;
map_event_level(critical)  -> fatal;
map_event_level(error)     -> error;
map_event_level(warning)   -> warning;
map_event_level(notice)    -> warning;
map_event_level(info)      -> info;
map_event_level(debug)     -> debug.

to_binary(Binary) when is_binary(Binary) ->
  Binary;
to_binary(String) when is_list(String) ->
  list_to_binary(String).

%% TODO: add limits
-spec parse_args(ArgsOrArity) -> Args when
    ArgsOrArity :: Args | Arity,
    Args        :: [{Key, Value}],
    Key         :: string(),
    Value       :: term(),
    Arity       :: non_neg_integer().
parse_args(Args) when is_list(Args) ->
  Indexes = lists:seq(0, max(0, length(Args) - 1)),
  Enumerated = lists:zip(Indexes, Args),
  Map =
    fun({Index, Arg}) ->
        Key = iolist_to_binary(io_lib:format("arg#~B", [Index])),
        {Key, Arg}
    end,
  lists:map(Map, Enumerated);
parse_args(_Arity) ->
  [].

-spec arity_to_integer(ArgsOrArity) -> Arity when
    ArgsOrArity :: Args | Arity,
    Args        :: list(),
    Arity       :: non_neg_integer().
arity_to_integer(Arity) when is_integer(Arity) ->
  Arity;
arity_to_integer(Args) ->
  length(Args).

-spec format_mfa(Module, Function, Arity) -> Formatted when
    Module    :: atom(),
    Function  :: atom(),
    Arity     :: non_neg_integer(),
    Formatted :: binary().
format_mfa(Module, Function, Arity) ->
  Formatted = io_lib:format(<<"~p:~p/~B">>, [Module, Function, Arity]),
  iolist_to_binary(Formatted).
