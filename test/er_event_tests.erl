-module(er_event_tests).

-include_lib("eunit/include/eunit.hrl").

to_map_test_() ->
  ServerName = <<"test_server">>,
  Environment = <<"staging">>,
  Release = <<"v0.1.0">>,
  Context = er_context:new(ServerName, Environment, Release, #{}, #{}, #{}, #{}, [], []),
  Message = <<"Message">>,
  Level = error,
  StacktraceWithArity =
    [{er_logger_handler,log,2,[{file,"eraven/src/er_logger_handler.erl"}, {line,12}]},
     {erl_eval,do_apply,6,[{file,"erl_eval.erl"},{line,684}]}],
  EventWithArity = er_event:new(Message, Level, StacktraceWithArity, Context),
  StacktraceWithArgs =
    [{er_logger_handler,log,["Test", 42],[{file,"eraven/src/er_logger_handler.erl"}, {line,12}]},
     {erl_eval,do_apply,6,[{file,"erl_eval.erl"},{line,684}]}],
  EventWithArgs = er_event:new(Message, Level, StacktraceWithArgs, Context),
  Platform = <<"erlang">>,
  Culprit = <<"er_logger_handler:log/2">>,
  HLoggerFilename = <<"eraven/src/er_logger_handler.erl">>,
  HLoggerFunction = <<"er_logger_handler:log/2">>,
  Args = [{<<"arg#0">>,"Test"},{<<"arg#1">>,42}],
  ErlEvalFrame = #{filename => <<"erl_eval.erl">>,
                   function => <<"erl_eval:do_apply/6">>,
                   lineno => 684,module => erl_eval,vars => []},
  [?_assertMatch(#{level := Level,message := Message,platform := Platform,
                   culprit := Culprit, server_name := ServerName, environment := Environment,
                   release := Release,
                   stacktrace :=
                     #{frames :=
                         [#{filename := HLoggerFilename,
                            function := HLoggerFunction,
                            lineno := 12,module := er_logger_handler,
                            vars := []},
                          ErlEvalFrame]}}, er_event:to_map(EventWithArity)),
   ?_assertMatch(#{level := Level,message := Message,platform := Platform,
                   culprit := Culprit, server_name := ServerName, environment := Environment,
                   release := Release,
                   stacktrace :=
                     #{frames :=
                         [#{filename := HLoggerFilename,
                            function := HLoggerFunction,
                            lineno := 12,module := er_logger_handler,
                            vars := Args},
                          ErlEvalFrame]}}, er_event:to_map(EventWithArgs))
  ].
