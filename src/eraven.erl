-module(eraven).

-include_lib("kernel/include/logger.hrl").

%% API exports
-export([start/2]).

%%====================================================================
%% API functions
%%====================================================================

start(_, _) ->
  Dsn = er_dsn:new("http://9f293de25b2c4a74b09ae731ba6aac58@localhost:9000/1"),
  logger:add_handler(eraven, er_logger_handler, #{dsn => Dsn}),
  ?LOG_ERROR("Test ~p", [?LINE]),
  {ok, self()}.

%%====================================================================
%% Internal functions
%%====================================================================
