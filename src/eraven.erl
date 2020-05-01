-module(eraven).

% API
-export([set_environment_context/4,
         set_process_extra/1,
         set_user_context/1,
         set_process_tags/1,
         set_request_context/5
        ]).

%%====================================================================
%% API functions
%%====================================================================

-spec set_environment_context(HandlerId,
                              ServerName,
                              Environment,
                              Release) -> ok | {error, term()} when
    HandlerId   :: logger:handler_id(),
    ServerName  :: string() | binary(),
    Environment :: string() | binary(),
    Release     :: string() | binary().
set_environment_context(HandlerId, ServerName, Environment, Release) ->
  EnvironmentContext = er_environment_context:new(ServerName, Environment, Release),
  logger:update_handler_config(HandlerId, config, #{environment_context => EnvironmentContext}).

-spec set_process_extra(Extra) -> ok when
    Extra  :: #{Key => Value},
    Key    :: atom() | binary(),
    Value  :: term().
set_process_extra(Extra) ->
  logger:update_process_metadata(#{eraven_process_extra => Extra}).

-spec set_user_context(UserData) -> ok | {error, term()} when
    UserData :: #{id         => InternalIdentifier,
                  username   => UserName,
                  email      => Email,
                  ip_address => IpAddress
                 },
    InternalIdentifier :: binary(),
    UserName           :: binary(),
    Email              :: binary(),
    IpAddress          :: inet:ip_address().
set_user_context(UserData) ->
  UserContext = er_user_context:new(UserData),
  logger:update_process_metadata(#{eraven_user_context => UserContext}).

-spec set_process_tags(Tags) -> ok when
    Tags  :: #{Key => Value},
    Key   :: atom() | binary(),
    Value :: atom() | binary().
set_process_tags(Tags) ->
  logger:update_process_metadata(#{eraven_process_tags => Tags}).

set_request_context(Method, Url, Headers, Env, Data) ->
  RequestContext = er_request_context:new(Method, Url, Headers, Env, Data),
  logger:update_process_metadata(#{eraven_request_context => RequestContext}).
