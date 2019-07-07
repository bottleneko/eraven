-module(er_context).

-record(er_context, {server_name :: binary(),
                     environment :: binary(),
                     exception   :: binary(),
                     release     :: binary(),
                     request     :: er_request_context:t(),
                     extra       :: map(),
                     user        :: map(),
                     tags        :: map(),
                     breadcrumbs :: term(),
                     fingerprint :: term()
                    }).

-opaque t() :: #er_context{}.
-export_type([t/0]).

% API
-export([new/9]).

% Accessors
-export([server_name/1, environment/1, release/1,
         request/1, extra/1, user/1, tags/1,
         breadcrumbs/1, fingerprint/1]).

%%%===================================================================
%%% API
%%%===================================================================

new(ServerName, Environment, Release, Request, Extra, User, Tags, Breadcrumps, Fingerprint) ->
  #er_context{
     server_name = to_binary(ServerName),
     environment = to_binary(Environment),
     release     = to_binary(Release),
     request     = Request,
     extra       = Extra,
     user        = User,
     tags        = Tags,
     breadcrumbs = Breadcrumps,
     fingerprint = Fingerprint
    }.

%%%===================================================================
%%% Accessors
%%%===================================================================

server_name(#er_context{server_name = ServerName}) ->
  ServerName.

environment(#er_context{environment = Environment}) ->
  Environment.

release(#er_context{release = Release}) ->
  Release.

request(#er_context{request = Request}) ->
  er_request_context:to_map(Request).

extra(#er_context{extra = Extra}) ->
  Extra.

user(#er_context{user = User}) ->
  User.

tags(#er_context{tags = Tags}) ->
  Tags.

breadcrumbs(#er_context{breadcrumbs = Breadcrumbs}) ->
  Breadcrumbs.

fingerprint(#er_context{fingerprint = Fingerprint}) ->
  Fingerprint.

%%%===================================================================
%%% Internal functions
%%%===================================================================

to_binary(Binary) when is_binary(Binary) ->
  Binary;
to_binary(String) when is_list(String) ->
  list_to_binary(String);
to_binary(Any) ->
  Any.
