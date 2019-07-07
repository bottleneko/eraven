-module(eraven).

-include_lib("kernel/include/logger.hrl").

%% API exports
-export([start/2]).

%%====================================================================
%% API functions
%%====================================================================

start(_, _) ->
  Dsn = er_dsn:new("http://9f293de25b2c4a74b09ae731ba6aac58@localhost:9000/1"),
  logger:add_handler(eraven, er_logger_handler, #{config => #{dsn => Dsn, json_encode_function => fun jsx:encode/1}, event_tags_key => event_tags}),
  set_environment_context(eraven, <<"test_server">>, <<"develop">>, <<"v0.1.0">>),
  set_user_context(#{id => <<"test_id">>, username => <<"some_user">>, email => <<"user@example.com">>, ip_address => {8,8,8,8}}),
  set_process_tags(#{<<"test_process_tag2">> => tag}),
  set_request_context(
    'POST',
    <<"https://localhost:9000/api/test">>,
    #{<<"test header">> => <<"test header value">>,
      <<"Content-Type">> => <<"application/json">>
     },
    #{<<"TEST_ENV">> => <<"TEST_ENV_VALUE">>},
    #{<<"body_key">> => <<"body_value">>}),
  try
    error(<<"Test error">>)
  catch
    Type:Reason:Stacktrace ->
      %% Configure event tag key name
      ?LOG_ERROR("Test ~p", [?LINE], #{type => Type, reason => Reason, stacktrace => Stacktrace, event_tags => #{testTag => <<"testTag">>}})
  end,
  {ok, self()}.

%%====================================================================
%% Internal functions
%%====================================================================

-spec set_environment_context(HandlerId, ServerName, Environment, Release) -> ok | {error, term()} when
    HandlerId   :: logger:handler_id(),
    ServerName  :: string() | binary(),
    Environment :: string() | binary(),
    Release     :: string() | binary().
set_environment_context(HandlerId, ServerName, Environment, Release) ->
  EnvironmentContext = er_environment_context:new(ServerName, Environment, Release),
  logger:update_handler_config(HandlerId, config, #{environment_context => EnvironmentContext}).

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
  logger:update_process_metadata(#{user_context => UserContext}).

-spec set_process_tags(Tags) -> ok when
    Tags  :: #{Key => Value},
    Key   :: atom() | binary(),
    Value :: atom() | binary().
set_process_tags(Tags) ->
  logger:update_process_metadata(#{eraven_process_tags => Tags}).

set_request_context(Method, Url, Headers, Env, Data) ->
  RequestContext = er_request_context:new(Method, Url, Headers, Env, Data),
  logger:update_process_metadata(#{eraven_request_context => RequestContext}).
