-module(er_client).

-define(SENTRY_VERSION, 7).
-define(SENTRY_CLIENT, "eraven/0.1.0").

-ifdef(TEST).
-define(HTTPC_OPTIONS, [{body_format, binary}]).
-else.
-define(HTTPC_OPTIONS, [{body_format, binary}, {sync, false}, {receiver, fun(_) -> ok end}]).
-endif.

-export([send_event/3]).

%%%===================================================================
%%% API
%%%===================================================================

-spec send_event(Event, Dsn, JsonEncodeFunction) -> ok when
    Event              :: er_event:t(),
    Dsn                :: er_dsn:t(),
    JsonEncodeFunction :: fun((Data) -> Json),
    Data               :: map(),
    Json               :: binary().
send_event(Event, Dsn, JsonEncodeFunction) ->
  Url = er_dsn:api_url(Dsn),
  Headers = authorization_headers(er_dsn:public_key(Dsn), er_dsn:secret_key(Dsn)),
  try
    Body = JsonEncodeFunction(er_event:to_map(Event)),
    CompressedBody = base64:encode(zlib:compress(Body)),
    Request = {Url, Headers, "application/octet-stream", CompressedBody},
  
    httpc:request(post, Request, [], ?HTTPC_OPTIONS)
  catch
    _Type:Exception:Stacktrace ->
      io:format("Eraven crash: ~p~n~p~n", [Exception, Stacktrace])
  end,
  ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec authorization_headers(PublicKey, SecretKey) -> Headers when
    PublicKey :: binary(),
    SecretKey :: binary() | undefined,
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
    SecretKey :: binary() | undefined,
    Format    :: string().
maybe_secret(undefined = _SecretKey) ->
  "";
maybe_secret(SecretKey) ->
  Format = ", sentry_secret=~s",
  io_lib:format(Format, [SecretKey]).
