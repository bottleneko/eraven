-module(er_event).

-record(er_event, {timestamp               :: non_neg_integer(),
                   message                 :: binary(),
                   level                   :: fatal | error | warning | info | debug,
                   platform = <<"erlang">> :: binary(),
                   exception               :: #{type  => error | throw,
                                                value => Reason :: term()}
                                            | undefined,
                   stacktrace              :: term()
                                            | undefined,
                   context                 :: term(),
                   module                  :: atom()
                                            | undefined,
                   line                    :: non_neg_integer()
                                            | undefined
                  }).

-opaque t() :: #er_event{}.
-export_type([t/0]).

-export([new/4, new/6, new/7]).
-export([to_map/1]).

%%%===================================================================
%%% API
%%%===================================================================

-spec new(Message, Level, Context, Timestamp) -> t() when
    Message   :: binary(),
    Level     :: logger:level(),
    Context   :: er_context:t(),
    Timestamp :: non_neg_integer().
new(Message, Level, Context, Timestamp) ->
  #er_event{
     timestamp = Timestamp,
     message   = to_binary(Message),
     level     = map_event_level(Level),
     context   = Context
   }.

-spec new(Message, Level, Module, Line, Context, Timestamp) -> t() when
    Message   :: binary(),
    Level     :: logger:level(),
    Module    :: atom(),
    Line      :: non_neg_integer(),
    Context   :: er_context:t(),
    Timestamp :: non_neg_integer().
new(Message, Level, Module, Line, Context, Timestamp) ->
  #er_event{
     timestamp = Timestamp,
     message   = to_binary(Message),
     level     = map_event_level(Level),
     module    = Module,
     line      = Line,
     context   = Context
   }.

-spec new(Message, Level, Type, Reason, Stacktrace, Context, Timestamp) -> t() when
    Message    :: binary(),
    Level      :: logger    :level(),
    Type       :: error | throw,
    Reason     :: term(),
    Stacktrace :: term(),
    Context    :: er_context:t(),
    Timestamp  :: non_neg_integer().
new(Message, Level, Type, Reason, Stacktrace, Context, Timestamp) ->
  #er_event{
     timestamp  = Timestamp,
     message    = to_binary(Message),
     level      = map_event_level(Level),
     exception  = #{type => Type, value => iolist_to_binary(io_lib:print(Reason))},
     stacktrace = Stacktrace,
     context    = Context
    }.

-spec to_map(t()) -> map().
to_map(Event) ->
  Context = Event#er_event.context,
  EnvironmentContext = er_context:environment_context(Context),
  RequestContext = er_context:request_context(Context),
  UserContext = er_context:user_context(Context),

  Map0 = #{
    timestamp   => Event#er_event.timestamp,
    message     => Event#er_event.message,
    level       => Event#er_event.level,
    platform    => Event#er_event.platform,
    server_name => call_or(fun er_environment_context:server_name/1, EnvironmentContext),
    environment => call_or(fun er_environment_context:environment/1, EnvironmentContext),
    release     => call_or(fun er_environment_context:release/1,     EnvironmentContext),
    request     => call_or(fun er_request_context:to_map/1, RequestContext),
    extra       => er_context:extra(Context),
    user        => call_or(fun er_user_context:to_map/1, UserContext),
    tags        => er_context:tags(Context),
    breadcrumbs => er_context:breadcrumbs(Context),
    fingerprint => er_context:fingerprint(Context)
   },

  Filter =
    fun(_Key, undefined = _Value) -> false;
       (_Key, _Value)             -> true
    end,
  Map1 = maps:filter(Filter, Map0),
  Map2 = maybe_culprit_from_location(Event, Map1),
  maybe_data_from_stacktrace(Event, Map2).

%%%===================================================================
%%% Sentry formatters
%%%===================================================================

maybe_culprit_from_location(#er_event{module = Module, line = Line} = _Event, Map) when
    Module =:= undefined,
    Line   =:= undefined ->
  Map;
maybe_culprit_from_location(#er_event{module = Module, line = Line} = _Event, Map) ->
  Formatted = io_lib:format("~p:~B", [Module, Line]),
  Map#{culprit => iolist_to_binary(Formatted)}.

maybe_data_from_stacktrace(#er_event{stacktrace = Stacktrace} = _Event, Map) when
    Stacktrace =:= undefined;
    Stacktrace =:= [] ->
  Map;
maybe_data_from_stacktrace(#er_event{stacktrace = Stacktrace} = Event, Map) ->
  {Module, Function, ArgsOrArity, _Location} = hd(Stacktrace),
  Culprit = format_mfa(Module, Function, arity_to_integer(ArgsOrArity)),
  Map#{culprit     => Culprit,
    exception   => Event#er_event.exception,
    stacktrace  => format_stacktrace(Stacktrace)
   }.

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
%% https://docs.sentry.io/enriching-error-data/context/#setting-the-level
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
-spec parse_args(ArityOrArgs) -> Args when
    ArityOrArgs :: Args | Arity,
    Arity       :: non_neg_integer(),
    Args        :: [{Key, Value}],
    Key         :: binary(),
    Value       :: any().
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

-dialyzer({nowarn_function, call_or/2}).
-spec call_or(Function :: any(), undefined) -> undefined;
             (Function, Argument)           -> Result when
    Function :: fun((Argument) -> Result),
    Argument :: term(),
    Result   :: term().
call_or(_Function, undefined) ->
  undefined;
call_or(Function, Argument) ->
  Function(Argument).

%%%===================================================================
%%% Tests
%%%===================================================================

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

map_event_level_test_() ->
  [?_assertEqual(fatal, map_event_level(emergency)),
   ?_assertEqual(fatal, map_event_level(alert)),
   ?_assertEqual(fatal, map_event_level(critical)),
   ?_assertEqual(error, map_event_level(error)),
   ?_assertEqual(warning, map_event_level(warning)),
   ?_assertEqual(warning, map_event_level(notice)),
   ?_assertEqual(info, map_event_level(info)),
   ?_assertEqual(debug, map_event_level(debug))
  ].

-endif.
