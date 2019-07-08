-module(er_environment_context_tests).

-include_lib("eunit/include/eunit.hrl").

-import(er_environment_context, [new/3, server_name/1, environment/1, release/1]).

%%%===================================================================
%%% Testcases
%%%===================================================================

getters_test_() ->
  ServerName = <<"test_server">>,
  Environment = <<"test_environment">>,
  Release = <<"test_release">>,

  EnvironmentContext = new(ServerName, Environment, Release),
  [?_assertEqual(ServerName, server_name(EnvironmentContext)),
   ?_assertEqual(Environment, environment(EnvironmentContext)),
   ?_assertEqual(Release, release(EnvironmentContext))
  ].

binary_test() ->
  ServerName = "test_server",
  Environment = "test_environment",
  Release = "test_release",

  EnvironmentContext = new(ServerName, Environment, Release),
  [?_assertEqual(list_to_binary(ServerName), server_name(EnvironmentContext)),
   ?_assertEqual(list_to_binary(Environment), environment(EnvironmentContext)),
   ?_assertEqual(list_to_binary(Release), release(EnvironmentContext))
  ].
