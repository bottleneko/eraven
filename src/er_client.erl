-module(er_client).

-define(SENTRY_VERSION, 7).
-define(SENTRY_CLIENT, "eraven/0.1.0").

-export([send_event/3]).

%%%===================================================================
%%% API
%%%===================================================================

send_event(Event, Dsn, JsonEncodeFunction) ->
  Url = er_dsn:api_url(Dsn),
  Headers = authorization_headers(er_dsn:public_key(Dsn), er_dsn:secret_key(Dsn)),
  Body = JsonEncodeFunction(er_event:to_map(Event)),
  httpc:request(post, {Url, Headers, "application/json", Body}, [], []).

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec authorization_headers(PublicKey, SecretKey) -> Headers when
    PublicKey :: string(),
    SecretKey :: string() | undefined,
    Headers   :: [{Field, Value}],
    Field     :: string(),
    Value     :: string().
authorization_headers(PublicKey, SecretKey) ->
  Timestamp = erlang:system_time(second),
  Format =
    "Sentry"
    " sentry_version=~B,"
    " sentry_client=~s,"
    " sentry_timestamp=~B,"
    " sentry_key=~s" ++
    maybe_secret(SecretKey),
  XSentryAuth = io_lib:format(Format, [?SENTRY_VERSION, ?SENTRY_CLIENT, Timestamp, PublicKey]),
  [{"User-Agent", ?SENTRY_CLIENT},
   {"X-Sentry-Auth", lists:flatten(XSentryAuth)}
  ].

-spec maybe_secret(SecretKey) -> Format when
    SecretKey :: string() | undefined,
    Format    :: string().
maybe_secret(undefined = _SecretKey) ->
  "";
maybe_secret(SecretKey) ->
  Format = ", sentry_secret=~s",
  io_lib:format(Format, [SecretKey]).
