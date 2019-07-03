-module(eraven).

-include_lib("kernel/include/logger.hrl").

%% API exports
-export([start/2]).

%%====================================================================
%% API functions
%%====================================================================

start(_, _) ->
  Dsn = er_dsn:new("http://9f293de25b2c4a74b09ae731ba6aac58@localhost:9000/1"),
  logger:add_handler(eraven, er_logger_handler, #{config => #{dsn => Dsn}}),
  A = set_environment_context(eraven, <<"test_server">>, <<"develop">>, <<"v0.1.0">>),
  io:format("AFTER SET CONTEXT: ~p~n", [A]),
  logger:update_process_metadata(#{username => <<"some_user">>}),
  try
    error(<<"Test error">>)
  catch
    Type:Reason:Stacktrace ->
      ?LOG_ERROR("Test ~p", [?LINE], #{type => Type, reason => Reason, stacktrace => Stacktrace})
  end,
  {ok, self()}.

%%====================================================================
%% Internal functions
%%====================================================================

-spec set_environment_context(HandlerId, ServerName, Environment, Release) -> ok | {error, term()} when
    HandlerId   :: logger:handler_id(),
    ServerName  :: string() | binary(),
    Environment :: string() | binary(),
    Release     :: string() | binary().
set_environment_context(HandlerId, ServerName, Environment, Release) ->
  EnvironmentContext = er_environment_context:new(ServerName, Environment, Release),
  logger:update_handler_config(HandlerId, config, #{environment_context => EnvironmentContext}).
