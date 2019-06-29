-module(er_event_tests).

-include_lib("eunit/include/eunit.hrl").

to_map_test_() ->
  StacktraceWithArity =
    [{er_logger_handler,log,2,[{file,"eraven/src/er_logger_handler.erl"}, {line,12}]},
     {erl_eval,do_apply,6,[{file,"erl_eval.erl"},{line,684}]}],
  StacktraceWithArgs =
    [{er_logger_handler,log,["Test", 42],[{file,"eraven/src/er_logger_handler.erl"}, {line,12}]},
     {erl_eval,do_apply,6,[{file,"erl_eval.erl"},{line,684}]}],
  EventWithArity = er_event:new("Message", error, StacktraceWithArity),
  EventWithArgs = er_event:new("Message", error, StacktraceWithArgs),
  [?_assertMatch(#{level := "error",message := "Message",platform := "erlang",
                   stacktrace :=
                     #{frames :=
                         [#{filename := "eraven/src/er_logger_handler.erl",
                            function := "er_logger_handler:log/2",
                            lineno := 12,module := er_logger_handler,
                            vars := []},
                          #{filename := "erl_eval.erl",
                            function := "erl_eval:do_apply/6",
                            lineno := 684,module := erl_eval,vars := []}]}}, er_event:to_map(EventWithArity)),
   ?_assertMatch(#{level := "error",message := "Message",platform := "erlang",
                   stacktrace :=
                     #{frames :=
                         [#{filename := "eraven/src/er_logger_handler.erl",
                            function := "er_logger_handler:log/2",
                            lineno := 12,module := er_logger_handler,
                            vars := [{"arg#0","Test"},{"arg#1",42}]},
                          #{filename := "erl_eval.erl",
                            function := "erl_eval:do_apply/6",
                            lineno := 684,module := erl_eval,vars := []}]}}, er_event:to_map(EventWithArgs))
  ].
