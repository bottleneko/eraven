-module(er_dsn).

-record(er_dsn, {scheme      :: http | https,
                 public_key  :: binary(),
                 private_key :: binary(),
                 hostname    :: binary(),
                 port        :: non_neg_integer(),
                 project_id  :: non_neg_integer()
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
      {PublicKey, PrivateKey} = parse_keys(UserInfo),
      #er_dsn{
         scheme      = Scheme,
         public_key  = PublicKey,
         private_key = PrivateKey,
         hostname    = Hostname,
         port        = Port,
         project_id  = list_to_integer(ProjectIdString, 10)
        };
    {error, _Reason} = Error ->
      Error
  end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec parse_keys(UserInfo) -> {PublicKey, PrivateKey} when
    UserInfo   :: string(),
    PublicKey  :: string(),
    PrivateKey :: string() | undefined.
parse_keys(UserInfo) ->
  case string:split(UserInfo, ":") of
    [PublicKey, PrivateKey] ->
      {PublicKey, PrivateKey};
    [PublicKey] ->
      {PublicKey, undefined}
  end.
