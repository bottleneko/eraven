-module(er_event_SUITE).

-include_lib("kernel/include/logger.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile([export_all, nowarn_export_all]).

%%%===================================================================
%%% CT callbacks
%%%===================================================================

all() ->
  [exception_with_stacktrace_test,
   exception_with_location_test,
   exception_test,
   exception_old_dsn_test
  ].

init_per_suite(Config) ->
  {ok, _} = bookish_spork:start_server(9090),
  Config.

end_per_suite(_Config) ->
  ok = bookish_spork:stop_server().

init_per_testcase(_Testcase, Config) ->
  bookish_spork:stub_request(),
  Config.

end_per_testcase(_Testcase, Config) ->
  logger:remove_handler(eraven),
  Config.

%%%===================================================================
%%% Testcases
%%%===================================================================

exception_with_stacktrace_test(_Config) ->
  {ok, Dsn} = er_dsn:new("http://9f293de25b2c4a74b09ae731ba6aac58@localhost:9090/1"),
  Config =
    #{config => #{dsn                  => Dsn,
                  json_encode_function => fun jsx:encode/1,
                  event_tags_key       => event_tags,
                  event_extra_key      => event_extra
                 },
      level     => all
     },
  logger:add_handler(eraven, er_logger_handler, Config),

  try
    erlang:error(<<"Test error">>)
  catch
    Type:Reason:Stacktrace ->
      ?LOG_ERROR("Test event", [], #{type => Type, reason => Reason, stacktrace => Stacktrace})
  end,

  {ok, Request} = bookish_spork:capture_request(),
  ?assertEqual('POST', bookish_spork_request:method(Request)),
  ?assertEqual("/api/1/store/", bookish_spork_request:uri(Request)),
  ?assertMatch(#{"connection"     := "keep-alive",
                 "content-length" := _ContentLength,
                 "content-type"   := "application/octet-stream",
                 "host"           := "localhost:9090",
                 "user-agent"     := "eraven/0.1.0",
                 "x-sentry-auth"  := _XSentryAuth
                }, bookish_spork_request:headers(Request)).

exception_with_location_test(_Config) ->
  {ok, Dsn} = er_dsn:new("http://9f293de25b2c4a74b09ae731ba6aac58@localhost:9090/1"),
  Config =
    #{config => #{dsn                  => Dsn,
                  json_encode_function => fun jsx:encode/1,
                  event_tags_key       => event_tags,
                  event_extra_key      => event_extra
                 },
      level     => all
     },
  logger:add_handler(eraven, er_logger_handler, Config),

  ?LOG_ERROR("Test event", []),

  {ok, Request} = bookish_spork:capture_request(),
  ?assertEqual('POST', bookish_spork_request:method(Request)),
  ?assertEqual("/api/1/store/", bookish_spork_request:uri(Request)),
  ?assertMatch(#{"connection"     := "keep-alive",
                 "content-length" := _ContentLength,
                 "content-type"   := "application/octet-stream",
                 "host"           := "localhost:9090",
                 "user-agent"     := "eraven/0.1.0",
                 "x-sentry-auth"  := _XSentryAuth
                }, bookish_spork_request:headers(Request)).


exception_test(_Config) ->
  {ok, Dsn} = er_dsn:new("http://9f293de25b2c4a74b09ae731ba6aac58@localhost:9090/1"),
  Config =
    #{config => #{dsn                  => Dsn,
                  json_encode_function => fun jsx:encode/1,
                  event_tags_key       => event_tags,
                  event_extra_key      => event_extra
                 },
      level     => all
     },
  logger:add_handler(eraven, er_logger_handler, Config),

  logger:error("Test event", []),

  {ok, Request} = bookish_spork:capture_request(),
  ?assertEqual('POST', bookish_spork_request:method(Request)),
  ?assertEqual("/api/1/store/", bookish_spork_request:uri(Request)),
  ?assertMatch(#{"connection"     := "keep-alive",
                 "content-length" := _ContentLength,
                 "content-type"   := "application/octet-stream",
                 "host"           := "localhost:9090",
                 "user-agent"     := "eraven/0.1.0",
                 "x-sentry-auth"  := _XSentryAuth
                }, bookish_spork_request:headers(Request)).

exception_old_dsn_test(_Config) ->
  {ok, Dsn} = er_dsn:new("http://9f293de25b2c4a74b09ae731ba6aac58:3c30a3ae29b440079ba31bbce62c34bb@localhost:9090/1"),
  Config =
    #{config => #{dsn                  => Dsn,
                  json_encode_function => fun jsx:encode/1,
                  event_tags_key       => event_tags,
                  event_extra_key      => event_extra
                 },
      level     => all
     },
  logger:add_handler(eraven, er_logger_handler, Config),

  logger:error("Test event"),

  {ok, Request} = bookish_spork:capture_request(),
  ?assertEqual('POST', bookish_spork_request:method(Request)),
  ?assertEqual("/api/1/store/", bookish_spork_request:uri(Request)),
  ?assertMatch(#{"connection"     := "keep-alive",
                 "content-length" := _ContentLength,
                 "content-type"   := "application/octet-stream",
                 "host"           := "localhost:9090",
                 "user-agent"     := "eraven/0.1.0",
                 "x-sentry-auth"  := _XSentryAuth
                }, bookish_spork_request:headers(Request)).
