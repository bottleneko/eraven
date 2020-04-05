-module(eraven_SUITE).

-include_lib("eunit/include/eunit.hrl").

-compile([export_all, nowarn_export_all]).

-define(TEST_DSN, "http://9f293de25b2c4a74b09ae731ba6aac58@localhost:9000/1").

%%%===================================================================
%%% CT callbacks
%%%===================================================================

all() ->
  [default_config,
   set_environment_context,
   set_process_extra,
   set_user_context,
   set_process_tags,
   set_request_context
  ].

init_per_testcase(set_environment_context, Config) ->
  logger:add_handler(eraven, er_logger_handler, #{config => #{dsn => ?TEST_DSN}}),
  Config;
init_per_testcase(_Testcase, Config) ->
  Config.

end_per_testcase(set_environment_context, Config) ->
  logger:remove_handler(eraven),
  Config;
end_per_testcase(_Testcase, Config) ->
  Config.

%%%===================================================================
%%% Testcases
%%%===================================================================
default_config(_Config) ->
  {ok, Dsn} = er_dsn:new(?TEST_DSN),
  Expected = maps:merge(er_logger_handler:default_config(), #{dsn => Dsn}),

  ct:pal("Check default config with string type dsn"),
  logger:add_handler(eraven, er_logger_handler, #{config => #{dsn => ?TEST_DSN}}),
  {ok, #{config := Config}} = logger:get_handler_config(eraven),
  ?assertEqual(Expected, Config),
  logger:remove_handler(eraven),
  
  ct:pal("Check default config with er_dsn:t() dsn"),
  logger:add_handler(eraven, er_logger_handler, #{config => #{dsn => Dsn}}),
  {ok, #{config := Config2}} = logger:get_handler_config(eraven),
  ?assertEqual(Expected, Config2),
  logger:remove_handler(eraven).

set_environment_context(_Config) ->
  ServerName = <<"test_server">>,
  Environment = <<"test_environment">>,
  Release = <<"test_release">>,

  EnvironmentContext = er_environment_context:new(ServerName, Environment, Release),

  eraven:set_environment_context(eraven, ServerName, Environment, Release),
  {ok, #{config := Config}} = logger:get_handler_config(eraven),
  ?assertMatch(#{environment_context := EnvironmentContext}, Config).

set_process_extra(_Config) ->
  ProcessExtra = #{<<"some_extra">> => <<"some_extra_value">>},

  eraven:set_process_extra(ProcessExtra),

  ?assertMatch(#{eraven_process_extra := ProcessExtra}, logger:get_process_metadata()).

set_user_context(_Config) ->
  InternalIdentifier = <<"test_id">>,
  UserName = <<"test_username">>,
  Email = <<"test_email@example.com">>,
  IpAddress = {8, 8, 8, 8},

  UserData = #{id => InternalIdentifier, username => UserName, email => Email, ip_address => IpAddress},

  eraven:set_user_context(UserData),

  UserContext = er_user_context:new(UserData),

  ?assertMatch(#{eraven_user_context := UserContext}, logger:get_process_metadata()).

set_process_tags(_Config) ->
  ProcessTags = #{<<"some_tag">> => <<"some_tag_value">>},

  eraven:set_process_tags(ProcessTags),

  ?assertMatch(#{eraven_process_tags := ProcessTags}, logger:get_process_metadata()).

set_request_context(_Config) ->
  Method = 'POST',
  Url = <<"https://localhost:9000/api/test">>,
  Headers =
    #{<<"test_header">> => <<"test_header_value">>,
      <<"Content-Type">> => <<"application/json">>
     },
  Env = #{<<"TEST_ENV">> => <<"TEST_ENV_VALUE">>},
  Data = #{<<"test_data_key">> => <<"test_data_value">>},

  eraven:set_request_context(Method, Url, Headers, Env, Data),

  RequestContext = er_request_context:new(Method, Url, Headers, Env, Data),
  ?assertMatch(#{eraven_request_context := RequestContext}, logger:get_process_metadata()).
