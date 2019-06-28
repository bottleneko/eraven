-module(er_dsn).

-record(er_dsn, {scheme     :: http | https,
                 public_key :: binary(),
                 secret_key :: binary(),
                 hostname   :: binary(),
                 port       :: non_neg_integer(),
                 project_id :: non_neg_integer()
                }).

-opaque t() :: #er_dsn{}.
-export_type([t/0]).

-export([new/1]).

%%%===================================================================
%%% API
%%%===================================================================

-spec new(DsnString :: string()) -> t().
new(DsnString) ->
  case http_uri:parse(DsnString) of
    {ok, {Scheme, UserInfo, Hostname, Port, "/" ++ ProjectIdString, _Qs}} ->
      {PublicKey, SecretKey} = parse_keys(UserInfo),
      #er_dsn{
         scheme     = Scheme,
         public_key = PublicKey,
         secret_key = SecretKey,
         hostname   = Hostname,
         port       = Port,
         project_id = list_to_integer(ProjectIdString, 10)
        };
    {error, _Reason} = Error ->
      Error
  end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec parse_keys(UserInfo) -> {PublicKey, SecretKey} when
    UserInfo  :: string(),
    PublicKey :: string(),
    SecretKey :: string() | undefined.
parse_keys(UserInfo) ->
  case string:split(UserInfo, ":") of
    [PublicKey, SecretKey] ->
      {PublicKey, SecretKey};
    [PublicKey] ->
      {PublicKey, undefined}
  end.
