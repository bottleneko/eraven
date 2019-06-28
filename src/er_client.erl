-module(er_client).

-define(SENTRY_VERSION, 7).
-define(SENTRY_CLIENT, "eraven/0.1.0").

-export([send_event/2]).

%%%===================================================================
%%% API
%%%===================================================================

send_event(_Event, _Config) ->
  {er_dsn, Scheme, PublicKey, PrivateKey, HostName, Port, ProjectId} = er_dsn:new("http://9f293de25b2c4a74b09ae731ba6aac58@localhost:9000/1"),
  Url = atom_to_list(Scheme) ++ "://" ++ HostName ++ ":" ++ integer_to_list(Port) ++ "/api/" ++ integer_to_list(ProjectId) ++ "/store/",
  Headers = authorization_headers(PublicKey, PrivateKey),
  Body = "{\"data\":\"test\"}",
  httpc:request(post, {Url, Headers, "application/json", Body}, [], []).

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec authorization_headers(PublicKey, PrivateKey) -> Headers when
    PublicKey  :: string(),
    PrivateKey :: string() | undefined,
    Headers    :: [{Field, Value}],
    Field      :: string(),
    Value      :: string().
authorization_headers(PublicKey, PrivateKey) ->
  Timestamp = erlang:system_time(second),
  Format =
    "Sentry"
    " sentry_version=~B,"
    " sentry_client=~s,"
    " sentry_timestamp=~B,"
    " sentry_key=~s" ++
    maybe_secret(PrivateKey),
  XSentryAuth = io_lib:format(Format, [?SENTRY_VERSION, ?SENTRY_CLIENT, Timestamp, PublicKey]),
  [{"User-Agent", ?SENTRY_CLIENT},
   {"X-Sentry-Auth", lists:flatten(XSentryAuth)}
  ].

-spec maybe_secret(PrivateKey) -> Format when
    PrivateKey :: string() | undefined,
    Format     :: string().
maybe_secret(undefined = _PrivateKey) ->
  "";
maybe_secret(PrivateKey) ->
  Format = ", sentry_secret=~s",
  io_lib:format(Format, [PrivateKey]).
