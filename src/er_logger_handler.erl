-module(er_logger_handler).

% Logger callbacks
-export([log/2, filter_config/1, changing_config/3, adding_handler/1, removing_handler/1]).

%%%===================================================================
%%% Logger callbacks
%%%===================================================================

log(#{msg   := Message,
      level := Level,
      meta  := Meta} = LogEvent,
    #{config := #{dsn                 := Dsn,
                  environment_context := EnvironmentContext}} = Config) ->
  try
    io:format("HERE~nEVENT: ~p~nCONFIG: ~p~n", [LogEvent, Config]),

    ServerName = er_environment_context:server_name(EnvironmentContext),
    Environment = er_environment_context:environment(EnvironmentContext),
    Release = er_environment_context:release(EnvironmentContext),

    UserContext = maps:get(user_context, Meta, undefined),
    UserContextData = er_user_context:to_map(UserContext),

    ProcessTags = maps:get(eraven_process_tags, Meta, #{}),
    EventTags = maps:get(tags, Meta, #{}),
    Tags = maps:merge(ProcessTags, EventTags),

    Context = er_context:new(ServerName, Environment, Release, #{}, #{}, UserContextData, Tags, [], []),
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

changing_config(update, OldConfig, NewConfig) ->
  OldParams = maps:get(config, OldConfig, #{}),
  NewParams = maps:get(config, NewConfig, #{}),
  {ok, NewConfig#{config => maps:merge(OldParams, NewParams)}}.

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

build_event(Message, Level, #{type := Type, reason := Reason, stacktrace := Stacktrace} = _Meta, Context) ->
  io:format(">>>>>>>>> ~p", [Reason]),
  er_event:new(Message, Level, Type, Reason, Stacktrace, Context);
build_event(Message, Level, #{mfa := {Module, _Function, _Arity}, line := Line} = _Meta, Context) ->
  er_event:new(Message, Level, Module, Line, Context);
build_event(Message, Level, _Meta, Context) ->
  er_event:new(Message, Level, Context).
