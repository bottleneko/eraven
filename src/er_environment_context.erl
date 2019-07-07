-module(er_environment_context).

-record(er_environment_context, {server_name :: binary(),
                                 environment :: binary(),
                                 release     :: binary()
                                }).

-opaque t() :: #er_environment_context{}.
-export_type([t/0]).

% API
-export([new/3]).

% Accessors
-export([server_name/1, environment/1, release/1]).

%%%===================================================================
%%% API
%%%===================================================================

new(ServerName, Environment, Release) ->
  #er_environment_context{
     server_name = to_binary(ServerName),
     environment = to_binary(Environment),
     release     = to_binary(Release)
    }.

%%%===================================================================
%%% Accessors
%%%===================================================================

-spec server_name(t()) -> binary().
server_name(#er_environment_context{server_name = ServerName}) ->
  ServerName.

-spec environment(t()) -> binary().
environment(#er_environment_context{environment = Environment}) ->
  Environment.

-spec release(t()) -> binary().
release(#er_environment_context{release = Release}) ->
  Release.

%%%===================================================================
%%% Internal functions
%%%===================================================================

to_binary(Binary) when is_binary(Binary) ->
  Binary;
to_binary(String) when is_list(String) ->
  list_to_binary(String).
