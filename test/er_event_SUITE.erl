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
   exception_old_dsn_test,

   report_test,
   report_with_params_test,
   report_otp_test,
   report_otp_report_test,
   report_undefined_test,
   report_wrong_test
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
  Config = #{config => #{dsn => Dsn}, level  => all},
  logger:add_handler(eraven, er_logger_handler, Config),

  try
    erlang:error(<<"Test error">>)
  catch
    Type:Reason:Stacktrace ->
      ?LOG_ERROR("Test event", [], #{type => Type, reason => Reason, stacktrace => Stacktrace})
  end,

  {ok, Request} = bookish_spork:capture_request(),
  ok = check_api_requirements(Request).

exception_with_location_test(_Config) ->
  {ok, Dsn} = er_dsn:new("http://9f293de25b2c4a74b09ae731ba6aac58@localhost:9090/1"),
  Config = #{config => #{dsn => Dsn}, level  => all},
  logger:add_handler(eraven, er_logger_handler, Config),

  ?LOG_ERROR("Test event", []),

  {ok, Request} = bookish_spork:capture_request(),
  ok = check_api_requirements(Request).

exception_test(_Config) ->
  {ok, Dsn} = er_dsn:new("http://9f293de25b2c4a74b09ae731ba6aac58@localhost:9090/1"),
  Config = #{config => #{dsn => Dsn}, level  => all},
  logger:add_handler(eraven, er_logger_handler, Config),

  logger:error("Test event", []),

  {ok, Request} = bookish_spork:capture_request(),
  ok = check_api_requirements(Request).

exception_old_dsn_test(_Config) ->
  {ok, Dsn} = er_dsn:new("http://"
                         "9f293de25b2c4a74b09ae731ba6aac58"
                         ":3c30a3ae29b440079ba31bbce62c34bb"
                         "@localhost:9090/1"),
  Config = #{config => #{dsn => Dsn}, level  => all},
  logger:add_handler(eraven, er_logger_handler, Config),

  logger:error("Test event"),

  {ok, Request} = bookish_spork:capture_request(),
  ok = check_api_requirements(Request).

report_test(_Config) ->
  {ok, Dsn} = er_dsn:new("http://9f293de25b2c4a74b09ae731ba6aac58@localhost:9090/1"),
  Config = #{config => #{dsn => Dsn}, level  => all},
  logger:add_handler(eraven, er_logger_handler, Config),

  logger:error(#{got => connection_request, id => 42, state => foo},
               #{report_cb => fun(R) -> {"~p", [R]} end}),

  {ok, Request} = bookish_spork:capture_request(),
  ok = check_api_requirements(Request),

  Body = decode(bookish_spork_request:body(Request)),

  ?assertMatch(#{message := <<"#{got => connection_request,id => 42,state => foo}">>}, Body).

report_with_params_test(_Config) ->
  {ok, Dsn} = er_dsn:new("http://9f293de25b2c4a74b09ae731ba6aac58@localhost:9090/1"),
  Config = #{config => #{dsn => Dsn}, level  => all},
  logger:add_handler(eraven, er_logger_handler, Config),

  logger:error(#{got => connection_request, id => 42, state => foo},
               #{report_cb => fun(R, _Params) -> io_lib:format("~p", [R]) end}),

  {ok, Request} = bookish_spork:capture_request(),
  ok = check_api_requirements(Request),

  Body = decode(bookish_spork_request:body(Request)),

  ?assertMatch(#{message := <<"#{got => connection_request,id => 42,state => foo}">>}, Body).

report_otp_test(_Config) ->
  {ok, Dsn} = er_dsn:new("http://9f293de25b2c4a74b09ae731ba6aac58@localhost:9090/1"),
  Config = #{config => #{dsn => Dsn}, level  => all},
  logger:add_handler(eraven, er_logger_handler, Config),

  logger:error(#{got => connection_request, id => 42, state => foo},
               #{report_cb => fun logger:format_otp_report/1}),

  {ok, Request} = bookish_spork:capture_request(),
  ok = check_api_requirements(Request),

  Body = decode(bookish_spork_request:body(Request)),

  ?assertMatch(#{message := <<"#{got => connection_request,id => 42,state => foo}">>}, Body).

report_otp_report_test(_Config) ->
  {ok, Dsn} = er_dsn:new("http://9f293de25b2c4a74b09ae731ba6aac58@localhost:9090/1"),
  Config = #{config => #{dsn => Dsn}, level  => all},
  logger:add_handler(eraven, er_logger_handler, Config),

  logger:error(#{report => #{got => connection_request, id => 42, state => foo},
                 label  => test_label},
               #{report_cb => fun logger:format_otp_report/1}),

  {ok, Request} = bookish_spork:capture_request(),
  ok = check_api_requirements(Request),

  Body = decode(bookish_spork_request:body(Request)),

  ?assertMatch(#{message := <<"    got: connection_request\n"
                              "    id: 42\n"
                              "    state: foo">>}, Body).

report_undefined_test(_Config) ->
  {ok, Dsn} = er_dsn:new("http://9f293de25b2c4a74b09ae731ba6aac58@localhost:9090/1"),
  Config = #{config => #{dsn => Dsn}, level  => all},
  logger:add_handler(eraven, er_logger_handler, Config),

  logger:error(#{report => #{got => connection_request, id => 42, state => foo},
                 label  => test_label},
               #{report_cb => undefined}),

  {ok, Request} = bookish_spork:capture_request(),
  ok = check_api_requirements(Request),

  Body = decode(bookish_spork_request:body(Request)),

  ?assertMatch(#{message := <<"Undefined report function">>}, Body).

report_wrong_test(_Config) ->
  {ok, Dsn} = er_dsn:new("http://9f293de25b2c4a74b09ae731ba6aac58@localhost:9090/1"),
  Config = #{config => #{dsn => Dsn}, level  => all},
  logger:add_handler(eraven, er_logger_handler, Config),

  logger:error(#{report => #{got => connection_request, id => 42, state => foo},
                 label  => test_label},
               #{report_cb => fun() -> 42 end}),

  {ok, Request} = bookish_spork:capture_request(),
  ok = check_api_requirements(Request),

  Body = decode(bookish_spork_request:body(Request)),

  ?assertMatch(#{message := <<"Wrong report function arity">>}, Body).

%%%===================================================================
%%% Internal functions
%%%===================================================================

decode(CompressedBody) ->
  DecompressedBody = zlib:uncompress(base64:decode(CompressedBody)),
  jsx:decode(DecompressedBody, [return_maps, {labels, atom}]).

check_api_requirements(Request) ->
  ?assertEqual('POST', bookish_spork_request:method(Request)),
  ?assertEqual("/api/1/store/", bookish_spork_request:uri(Request)),
  ?assertMatch(#{"connection"     := "keep-alive",
                 "content-length" := _ContentLength,
                 "content-type"   := "application/octet-stream",
                 "host"           := "localhost:9090",
                 "user-agent"     := "eraven/0.1.0",
                 "x-sentry-auth"  := _XSentryAuth
                }, bookish_spork_request:headers(Request)),
  ok.
