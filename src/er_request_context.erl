-module(er_request_context).

-record(er_request_context, {method       :: 'GET' | 'HEAD' | 'POST' | 'PUT' | 'DELETE' | 'CONNECT' | 'OPTIONS' | 'TRACE', % https://tools.ietf.org/html/rfc7231
                             url          :: binary(),
                             headers      :: #{binary() => binary()},
                             env          :: #{binary() => binary()},
                             data         :: term()
                            }).

-opaque t() :: #er_request_context{}.
-export_type([t/0]).

% API
-export([new/5, to_map/1]).

%%%===================================================================
%%% API
%%%===================================================================

new(Method, Url, Headers, Env, Data) ->
  #er_request_context{
     method       = Method,
     url          = Url,
     headers      = Headers,
     env          = Env,
     data         = Data
    }.

to_map(#er_request_context{method = Method, url = Url, headers = Headers, env = Env, data = Data}) ->
  SerializedHeaders = lists:map(fun tuple_to_list/1, maps:to_list(Headers)),
  #{method                => Method,
    url                   => Url,
    headers               => SerializedHeaders,
    env                   => Env,
    data                  => Data
   }.
