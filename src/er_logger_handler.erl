-module(er_logger_handler).

% Logger callbacks
-export([log/2, filter_config/1, changing_config/3, adding_handler/1, removing_handler/1]).

%%%===================================================================
%%% Logger callbacks
%%%===================================================================

log(#{msg := Message, level := Level, meta := Meta} = LogEvent, #{dsn := Dsn} = Config) ->
  try
    io:format("HERE~nEVENT: ~p~nCONFIG: ~p~n", [LogEvent, Config]),
    Context = er_context:new("server_name", "develop", "v0.1.0", #{}, #{}, #{}, #{}, [], []),
    Event = build_event(format_message(Message), Level, Meta, Context),
    er_client:send_event(Event, Dsn)
  catch
    Type:Error:Stacktrace ->
      io:format("~p~n~p~n~p~n", [Type, Error, Stacktrace])
  end;
log(LogEvent, Config) ->
  io:format("HERE2~nEVENT: ~p~nCONFIG: ~p~n", [LogEvent, Config]).

filter_config(Config) ->
  Config.

changing_config(_SetOrUpdate, _OldConfig, NewConfig) ->
  NewConfig.

adding_handler(Config) ->
  {ok, Config}.

removing_handler(_Config) ->
  ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

format_message({string, Message}) ->
  Message;
format_message({Format, Data}) ->
  Formatted = io_lib:format(Format, Data),
  iolist_to_binary(Formatted).

build_event(Message, Level, #{mfa := {Module, _Function, _Arity}, line := Line} = _Meta, Context) ->
  er_event:new(Message, Level, Module, Line, Context);
build_event(Message, Level, _Meta, Context) ->
  er_event:new(Message, Level, Context).
