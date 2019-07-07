-module(er_context).

-record(er_context, {environment_context :: er_environment_context:t(),
                     exception           :: binary(),
                     request_context     :: er_request_context:t(),
                     extra               :: map(),
                     user                :: map(),
                     tags                :: map(),
                     breadcrumbs         :: term(),
                     fingerprint         :: term()
                    }).

-opaque t() :: #er_context{}.
-export_type([t/0]).

% API
-export([new/7]).

% Accessors
-export([environment_context/1, request_context/1,
         extra/1, user/1, tags/1,
         breadcrumbs/1, fingerprint/1]).

%%%===================================================================
%%% API
%%%===================================================================

new(EnvironmentContext, Request, Extra, User, Tags, Breadcrumps, Fingerprint) ->
  #er_context{
     environment_context = EnvironmentContext,
     request_context     = Request,
     extra               = Extra,
     user                = User,
     tags                = Tags,
     breadcrumbs         = Breadcrumps,
     fingerprint         = Fingerprint
    }.

%%%===================================================================
%%% Accessors
%%%===================================================================

environment_context(#er_context{environment_context = EnvironmentContext}) ->
  EnvironmentContext.

request_context(#er_context{request_context = Request}) ->
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
