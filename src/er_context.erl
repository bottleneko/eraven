-module(er_context).

-record(er_context, {environment_context :: er_environment_context:t(),
                     request_context     :: er_request_context:t(),
                     extra               :: map(),
                     user_context        :: er_user_context:t(),
                     tags                :: map(),
                     breadcrumbs         :: any(),
                     fingerprint         :: [binary()]
                    }).

-opaque t() :: #er_context{}.
-export_type([t/0]).

% API
-export([new/7]).

% Accessors
-export([environment_context/1, request_context/1,
         extra/1, user_context/1, tags/1,
         breadcrumbs/1, fingerprint/1]).

%%%===================================================================
%%% API
%%%===================================================================

-spec new(EnvironmentContext,
          RequestContext,
          Extra,
          UserContext,
          Tags,
          Breadcrumbs,
          Fingerprint) -> t() when
    EnvironmentContext :: er_environment_context:t(),
    RequestContext     :: er_request_context:t(),
    Extra              :: map(),
    UserContext        :: er_user_context:t(),
    Tags               :: map(),
    Breadcrumbs        :: any(),
    Fingerprint        :: [binary()].
new(EnvironmentContext, RequestContext, Extra, UserContext, Tags, Breadcrumbs, Fingerprint) ->
  #er_context{
     environment_context = EnvironmentContext,
     request_context     = RequestContext,
     extra               = Extra,
     user_context        = UserContext,
     tags                = Tags,
     breadcrumbs         = Breadcrumbs,
     fingerprint         = Fingerprint
    }.

%%%===================================================================
%%% Accessors
%%%===================================================================

environment_context(#er_context{environment_context = EnvironmentContext}) ->
  EnvironmentContext.

request_context(#er_context{request_context = RequestContext}) ->
  RequestContext.

extra(#er_context{extra = Extra}) ->
  Extra.

user_context(#er_context{user_context = UserContext}) ->
  UserContext.

tags(#er_context{tags = Tags}) ->
  Tags.

breadcrumbs(#er_context{breadcrumbs = Breadcrumbs}) ->
  Breadcrumbs.

fingerprint(#er_context{fingerprint = Fingerprint}) ->
  Fingerprint.
