-module(er_logger_handler).

% Logger callbacks
-export([log/2, filter_config/1, changing_config/3, adding_handler/1, removing_handler/1]).

%%%===================================================================
%%% Logger callbacks
%%%===================================================================

log(_LogEvent, _Config) ->
  io:format("HERE").

filter_config(Config) ->
  Config.

changing_config(_SetOrUpdate, _OldConfig, NewConfig) ->
  NewConfig.

adding_handler(Config) ->
  {ok, Config}.

removing_handler(_Config) ->
  ok.
