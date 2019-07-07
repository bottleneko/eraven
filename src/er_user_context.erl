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
new(UserData) ->
  Filtered = maps:with([id, username, email, ip_address], UserData),
  Size = maps:size(Filtered),
  if Size > 0 ->
      #er_user_context{
         id         = maps:get(id, Filtered, undefined),
         username   = maps:get(username, Filtered, undefined),
         email      = maps:get(email, Filtered, undefined),
         ip_address = maps:get(ip_address, Filtered, undefined)
        };
    true ->
      undefined
  end.

-spec to_map(UserContext) -> map() when
    UserContext :: t() | undefined.
to_map(undefined) ->
  #{};
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
