-module(er_dsn_tests).

-include_lib("eunit/include/eunit.hrl").

new_test_() ->
  [?_assertEqual({ok, {er_dsn,http,"9f293de25b2c4a74b09ae731ba6aac58",undefined,
                       "localhost",9000,1}},
                 er_dsn:new("http://9f293de25b2c4a74b09ae731ba6aac58@localhost:9000/1")),
   ?_assertEqual({ok, {er_dsn,http,"9f293de25b2c4a74b09ae731ba6aac58",
                       "3c30a3ae29b440079ba31bbce62c34bb","localhost",9000, 1}},
                 er_dsn:new("http://9f293de25b2c4a74b09ae731ba6aac58:3c30a3ae29b440079ba31bbce62c34bb@localhost:9000/1")),
   ?_assertEqual({error,
                  {malformed_url,http,
                   "http://9f293de25b2c4a74b09ae731ba6aac58:3c30a3ae29b440079ba31bbce62c34bb@localhost:9000:1/1"}},
                 er_dsn:new("http://9f293de25b2c4a74b09ae731ba6aac58:3c30a3ae29b440079ba31bbce62c34bb@localhost:9000:1/1")),
   ?_assertError(badarg, er_dsn:new("http://9f293de25b2c4a74b09ae731ba6aac58:3c30a3ae29b440079ba31bbce62c34bb@localhost:9000/a"))
  ].

public_key_test_() ->
  {ok, Dsn} = er_dsn:new("http://9f293de25b2c4a74b09ae731ba6aac58@localhost:9000/1"),
  {ok, OldDsn} = er_dsn:new("http://9f293de25b2c4a74b09ae731ba6aac58:3c30a3ae29b440079ba31bbce62c34bb@localhost:9000/1"),
  [?_assertEqual("9f293de25b2c4a74b09ae731ba6aac58", er_dsn:public_key(Dsn)),
   ?_assertEqual("9f293de25b2c4a74b09ae731ba6aac58", er_dsn:public_key(OldDsn))
  ].

secret_key_test_() ->
  {ok, Dsn} = er_dsn:new("http://9f293de25b2c4a74b09ae731ba6aac58@localhost:9000/1"),
  {ok, OldDsn} = er_dsn:new("http://9f293de25b2c4a74b09ae731ba6aac58:3c30a3ae29b440079ba31bbce62c34bb@localhost:9000/1"),
  [?_assertEqual(undefined, er_dsn:secret_key(Dsn)),
   ?_assertEqual("3c30a3ae29b440079ba31bbce62c34bb", er_dsn:secret_key(OldDsn))
  ].

api_url_test_() ->
  {ok, Dsn} = er_dsn:new("http://9f293de25b2c4a74b09ae731ba6aac58@localhost:9000/1"),
  {ok, OtherValidDsn} = er_dsn:new("https://9f293de25b2c4a74b09ae731ba6aac58@localhost:9000/1/"),
  {ok, OldDsn} = er_dsn:new("http://9f293de25b2c4a74b09ae731ba6aac58:3c30a3ae29b440079ba31bbce62c34bb@localhost:9000/1"),
  [?_assertEqual("http://localhost:9000/api/1/store/", er_dsn:api_url(Dsn)),
   ?_assertEqual("http://localhost:9000/api/1/store/", er_dsn:api_url(OldDsn)),
   ?_assertEqual("https://localhost:9000/api/1/store/", er_dsn:api_url(OtherValidDsn))
  ].
