-module(eraven).

-include_lib("kernel/include/logger.hrl").

%% API exports
-export([start/2]).

%%====================================================================
%% API functions
%%====================================================================

start(_, _) ->
  Dsn = er_dsn:new("http://9f293de25b2c4a74b09ae731ba6aac58@localhost:9000/1"),
  logger:add_handler(eraven, er_logger_handler, #{config => #{dsn => Dsn}}),
  set_environment_context(eraven, <<"test_server">>, <<"develop">>, <<"v0.1.0">>),
  UC = set_user_context(#{id => <<"test_id">>, username => <<"some_user">>, email => <<"user@example.com">>, ip_address => {8,8,8,8}}),
  io:format("ASUC ~p", [UC]),
  try
    error(<<"Test error">>)
  catch
    Type:Reason:Stacktrace ->
      ?LOG_ERROR("Test ~p", [?LINE], #{type => Type, reason => Reason, stacktrace => Stacktrace})
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
  io:format("UCI ~p", [UserContext]),
  logger:update_process_metadata(#{user_context => UserContext}).
