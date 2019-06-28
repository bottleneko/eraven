-module(er_dsn_tests).

-include_lib("eunit/include/eunit.hrl").

new_test_() ->
  [?_assertEqual({er_dsn,http,"9f293de25b2c4a74b09ae731ba6aac58",undefined,
                  "localhost",9000,1},
                 er_dsn:new("http://9f293de25b2c4a74b09ae731ba6aac58@localhost:9000/1")),
   ?_assertEqual({er_dsn,http,"9f293de25b2c4a74b09ae731ba6aac58",
                  "3c30a3ae29b440079ba31bbce62c34bb","localhost",9000, 1},
                 er_dsn:new("http://9f293de25b2c4a74b09ae731ba6aac58:3c30a3ae29b440079ba31bbce62c34bb@localhost:9000/1")),
   ?_assertEqual({error,
                  {malformed_url,http,
                   "http://9f293de25b2c4a74b09ae731ba6aac58:3c30a3ae29b440079ba31bbce62c34bb@localhost:9000:1/1"}},
                 er_dsn:new("http://9f293de25b2c4a74b09ae731ba6aac58:3c30a3ae29b440079ba31bbce62c34bb@localhost:9000:1/1")),
   ?_assertError(badarg, er_dsn:new("http://9f293de25b2c4a74b09ae731ba6aac58:3c30a3ae29b440079ba31bbce62c34bb@localhost:9000/a"))
  ].
