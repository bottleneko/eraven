-module(er_user_context_tests).

-include_lib("eunit/include/eunit.hrl").

-import(er_user_context, [new/1, to_map/1]).

%%%===================================================================
%%% Testcases
%%%===================================================================

new_undefined_test_() ->
  [?_assertEqual(undefined, new(#{})),
   ?_assertEqual(undefined, new(#{unknown_field => unknown_field_value}))
  ].

new_test_() ->
  InternalIdentifier = <<"test_id">>,
  UserName = <<"test_username">>,
  Email = <<"test_email@example.com">>,
  IpAddress = {8, 8, 8, 8},

  UserData = #{id => InternalIdentifier, username => UserName, email => Email, ip_address => IpAddress},

  [?_assertNotMatch(undefined, new(maps:without([], UserData))),
   ?_assertNotMatch(undefined, new(maps:without([id], UserData))),
   ?_assertNotMatch(undefined, new(maps:without([id, username], UserData))),
   ?_assertNotMatch(undefined, new(maps:without([id, username, email], UserData))),
   ?_assertNotMatch(undefined, new(maps:without([id, username, ip_address], UserData)))
  ].

to_map_test_() ->
  InternalIdentifier = <<"test_id">>,
  UserName = <<"test_username">>,
  Email = <<"test_email@example.com">>,
  IpAddress = {8, 8, 8, 8},

  UserData = #{id => InternalIdentifier, username => UserName, email => Email, ip_address => IpAddress},
  MapUserData = #{id => InternalIdentifier, username => UserName, email => Email, ip_address => list_to_binary(inet:ntoa(IpAddress))},

  [?_assertEqual(maps:without([], MapUserData),
                 to_map(new(maps:without([], UserData)))
                ),
   ?_assertEqual(maps:without([id], MapUserData),
                 to_map(new(maps:without([id], UserData)))
                ),
   ?_assertEqual(maps:without([id, username], MapUserData),
                 to_map(new(maps:without([id, username], UserData)))
                ),
   ?_assertEqual(maps:without([id, username, email], MapUserData),
                 to_map(new(maps:without([id, username, email], UserData)))
                ),
   ?_assertEqual(maps:without([id, username, ip_address], MapUserData),
                 to_map(new(maps:without([id, username, ip_address], UserData)))
                ),
   ?_assertEqual(#{}, to_map(undefined))
  ].
