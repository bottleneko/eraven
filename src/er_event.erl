-module(er_event).

-record(er_event, {timestamp               :: non_neg_integer(),
                   message                 :: binary(),
                   level                   :: emergency | alert | critical | error | warning | notice | info | debug, % https://tools.ietf.org/html/rfc5424
                   platform = <<"erlang">> :: binary(),
%                   exception           :: binary(),
                   stacktrace              :: term(),
                   context                 :: term()
%                   modules             :: [atom()]
                  }).

-opaque t() :: #er_event{}.

-export_type([t/0]).

-export([new/4]).
-export([to_map/1]).

%%%===================================================================
%%% API
%%%===================================================================

new(Message, Level, Stacktrace, Context) ->
  #er_event{
     timestamp   = erlang:system_time(second),
     message     = to_binary(Message),
     level       = Level,
     stacktrace  = Stacktrace,
     context     = Context
    }.

to_map(Event) ->
  Stacktrace = Event#er_event.stacktrace,
  Context = Event#er_event.context,

  #{culprit     => culprit_from_stacktrace(Stacktrace),
    timestamp   => Event#er_event.timestamp,
    message     => Event#er_event.message,
    level       => Event#er_event.level,
    platform    => Event#er_event.platform,
    stacktrace  => format_stacktrace(Stacktrace),
    server_name => er_context:server_name(Context),
    environment => er_context:environment(Context),
%    exception   => #{type => <<"error">>, value => <<"test">>},
    release     => er_context:release(Context),
    request     => er_context:request(Context),
    extra       => er_context:extra(Context),
    user        => er_context:user(Context),
    tags        => er_context:tags(Context),
    breadcrumbs => er_context:breadcrumbs(Context),
    fingerprint => er_context:fingerprint(Context)
   }.

%%%===================================================================
%%% Sentry formatters
%%%===================================================================

culprit_from_stacktrace([StacktraceLine | _RestStacktrace] = _Stacktrace) ->
  {Module, Function, ArgsOrArity, _Location} = StacktraceLine,
  format_mfa(Module, Function, arity_to_integer(ArgsOrArity)).

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
