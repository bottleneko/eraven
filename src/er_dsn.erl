-module(er_dsn).

-record(er_dsn, {scheme     :: http | https,
                 public_key :: binary(),
                 secret_key :: binary() | undefined,
                 hostname   :: binary(),
                 port       :: non_neg_integer(),
                 project_id :: non_neg_integer()
                }).

-opaque t() :: #er_dsn{}.
-export_type([t/0]).

-export([new/1]).
-export([public_key/1, secret_key/1]).
-export([api_url/1]).

%%%===================================================================
%%% API
%%%===================================================================

-spec new(DsnString) -> {ok, t()} | {error, Reason} when
    DsnString :: string(),
    Reason    :: term().
new(DsnString) when is_list(DsnString) ->
  case uri_string:parse(DsnString) of
    #{scheme := Scheme, userinfo := UserInfo, host := Hostname, path := Path} = URIMap ->
      DefaultPort = case Scheme of
        "http" -> 80;
        "https" -> 443
      end,
      Port = maps:get(port, URIMap, DefaultPort),
      {PublicKey, SecretKey} = parse_keys(UserInfo),
      Dsn =
        #er_dsn{
           scheme     = Scheme,
           public_key = PublicKey,
           secret_key = SecretKey,
           hostname   = Hostname,
           port       = Port,
           project_id = parse_project_id(Path)
          },
      {ok, Dsn};
    {error, _Reason} = Error ->
      Error
  end;
new(Dsn) when is_record(Dsn, er_dsn) ->
  {ok, Dsn}.

-spec public_key(Dsn) -> PublicKey when
    Dsn       :: t(),
    PublicKey :: binary().
public_key(#er_dsn{public_key = PublicKey}) ->
  PublicKey.

-spec secret_key(Dsn) -> SecretKey when
    Dsn       :: t(),
    SecretKey :: binary() | undefined.
secret_key(#er_dsn{secret_key = SecretKey}) ->
  SecretKey.

-spec api_url(Dsn) -> ProjectId when
    Dsn       :: t(),
    ProjectId :: string().
api_url(#er_dsn{scheme = Scheme, hostname = HostName, port = Port, project_id = ProjectId}) ->
  Chars = io_lib:format("~p://~s:~B/api/~B/store/", [Scheme, HostName, Port, ProjectId]),
  lists:flatten(Chars).

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec parse_keys(UserInfo) -> {PublicKey, SecretKey} when
    UserInfo  :: string(),
    PublicKey :: binary(),
    SecretKey :: binary() | undefined.
parse_keys(UserInfo) ->
  case string:split(UserInfo, ":") of
    [PublicKey, SecretKey] ->
      {PublicKey, SecretKey};
    [PublicKey] ->
      {PublicKey, undefined}
  end.

-spec parse_project_id(Path) -> ProjectId when
    Path      :: string(),
    ProjectId :: non_neg_integer().
parse_project_id(Path) ->
  ProjectIdString = string:strip(Path, both, $/),
  list_to_integer(ProjectIdString).
