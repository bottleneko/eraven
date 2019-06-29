-module(er_event).

-record(er_event, {
%                   id                  :: non_neg_integer(),
%                   culprit             :: term(),
                   timestamp           :: non_neg_integer(),
                   message             :: binary(),
%                   tags                :: map(),
                   level               :: emergency | alert | critical | error | warning | notice | info | debug, % https://tools.ietf.org/html/rfc5424
                   platform = "erlang" :: string(),
%                   server_name         :: binary(),
%                   environment         :: binary(),
%                   exception           :: binary(),
%                   release             :: binary(),
                   stacktrace          :: term()
%                   request             :: term(),
%                   extra               :: term(),
%                   user                :: term(),
%                   breadcrumbs         :: term(),
%                   fingerprint         :: term(),
%                   modules             :: [atom()]
                  }).

-opaque t() :: #er_event{}.

-export_type([t/0]).

-export([new/3]).
-export([to_map/1]).

%%%===================================================================
%%% API
%%%===================================================================

new(Message, Level, Stacktrace) ->
  #er_event{
     timestamp  = erlang:system_time(second),
     message    = Message,
     level      = Level,
     stacktrace = Stacktrace
    }.

to_map(Event) ->
  #{timestamp  => Event#er_event.timestamp,
    message    => Event#er_event.message,
    level      => atom_to_list(Event#er_event.level),
    platform   => Event#er_event.platform,
    stacktrace => format_stacktrace(Event#er_event.stacktrace)
   }.

%%%===================================================================
%%% Internal functions
%%%===================================================================

format_stacktrace(Stacktrace) ->
  Map =
    fun({Module, Function, ArgsOrArity, Location}) ->
        Arity = arity_to_integer(ArgsOrArity),

        #{filename => proplists:get_value(file, Location),
          function => format_mfa(Module, Function, Arity),
          module   => Module,
          lineno   => proplists:get_value(line, Location),
          vars     => parse_args(ArgsOrArity)
         }
    end,
  #{frames => lists:map(Map, Stacktrace)}.

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
        Key = io_lib:format("arg#~B", [Index]),
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
    Formatted :: string().
format_mfa(Module, Function, Arity) ->
  Formatted = io_lib:format("~p:~p/~B", [Module, Function, Arity]),
  lists:flatten(Formatted).
