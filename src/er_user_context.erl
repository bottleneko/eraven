-module(er_user_context).

-record(er_user_context, {id         :: binary(),
                          username   :: binary(),
                          email      :: binary(),
                          ip_address :: inet:address()
                         }).

-opaque t() :: #er_user_context{}.
-export_type([t/0]).

-export([new/1]).
-export([to_map/1]).

%%%===================================================================
%%% API
%%%===================================================================

-spec new(UserData) -> t() | undefined when
    UserData :: #{id         => InternalIdentifier,
                  username   => UserName,
                  email      => Email,
                  ip_address => IpAddress
                 },
    InternalIdentifier :: binary(),
    UserName           :: binary(),
    Email              :: binary(),
    IpAddress          :: inet:ip_address().
new(UserData) when
    is_map_key(id, UserData);
    is_map_key(username, UserData);
    is_map_key(email, UserData);
    is_map_key(ip_address, UserData) ->
  #er_user_context{
     id         = maps:get(id, UserData, undefined),
     username   = maps:get(username, UserData, undefined),
     email      = maps:get(email, UserData, undefined),
     ip_address = maps:get(ip_address, UserData, undefined)
    };
new(_UserData) ->
  undefined.

-spec to_map(UserContext) -> map() when
    UserContext :: t().
to_map(UserData) ->
  Fields = record_info(fields, er_user_context),
  Data = tl(tuple_to_list(UserData)),
  Proplist = lists:zip(Fields, Data),
  Filter =
    fun({_Key, undefined = _Value}) -> false;
       (_Property)                  -> true
    end,
  Filtered = lists:filter(Filter, Proplist),
  Map = maps:from_list(Filtered),
  maybe_serialize_ip_address(Map).

%%%===================================================================
%%% Internal functions
%%%===================================================================

maybe_serialize_ip_address(#{ip_address := IpAddress} = Map) ->
  Map#{ip_address => list_to_binary(inet:ntoa(IpAddress))};
maybe_serialize_ip_address(Map) ->
  Map.
