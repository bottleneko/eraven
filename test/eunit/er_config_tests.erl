-module(er_config_tests).

-include_lib("eunit/include/eunit.hrl").


new_test_() ->
  DsnString = "http://9f293de25b2c4a74b09ae731ba6aac58@localhost:9000/1",
  {ok, Dsn} = er_dsn:new(DsnString),
  WrongDsnString = "http://9f293de25b2c4a74b09ae731ba6aac58:3c30a3ae29b440079ba31bbce62c34bb@localhost:9000:1/1",

  [?_assertEqual(er_config:new(#{dsn => DsnString}), er_config:new(#{dsn => Dsn})),
   ?_assertEqual({error, <<"Missing required property `dsn`">>}, er_config:new(#{})),
   ?_assertEqual({error,
                  {malformed_url,http,
                   "http://9f293de25b2c4a74b09ae731ba6aac58:3c30a3ae29b440079ba31bbce62c34bb@localhost:9000:1/1"}},
                 er_config:new(#{dsn => WrongDsnString}))
  ].
