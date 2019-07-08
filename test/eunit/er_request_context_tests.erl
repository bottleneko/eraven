-module(er_request_context_tests).

-include_lib("eunit/include/eunit.hrl").

-import(er_request_context, [new/5, to_map/1]).

%%%===================================================================
%%% Testcases
%%%===================================================================

to_map_test() ->
  Method = 'POST',
  Url = <<"https://localhost:9000/api/test">>,
  Headers =
    #{<<"test_header">> => <<"test_header_value">>,
      <<"Content-Type">> => <<"application/json">>
     },
  Env = #{<<"TEST_ENV">> => <<"TEST_ENV_VALUE">>},
  Data = #{<<"test_data_key">> => <<"test_data_value">>},

  RequestContext = new(Method, Url, Headers, Env, Data),
  RequestContextMap =
    #{method  => Method,
      url     => Url,
      headers => [[<<"Content-Type">>,<<"application/json">>], [<<"test_header">>,<<"test_header_value">>]],
      env     => Env,
      data    => Data
     },
  ?assertEqual(RequestContextMap, to_map(RequestContext)).
