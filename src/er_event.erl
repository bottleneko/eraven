-module(er_event).

-record(er_event, {
%                   id                  :: non_neg_integer(),
%                   culprit             :: term(),
                   timestamp           :: non_neg_integer(),
                   message             :: binary(),
%                   tags                :: map(),
                   level               :: emergency | alert | critical | error | warning | notice | info | debug, % https://tools.ietf.org/html/rfc5424
                   platform = "erlang" :: string()
%                   server_name         :: binary(),
%                   environment         :: binary(),
%                   exception           :: binary(),
%                   release             :: binary(),
%                   stacktrace          :: term(),
%                   request             :: term(),
%                   extra               :: term(),
%                   user                :: term(),
%                   breadcrumbs         :: term(),
%                   fingerprint         :: term(),
%                   modules             :: [atom()]
                  }).

-opaque t() :: #er_event{}.

-export_type([t/0]).

-export([new/2]).

new(Message, Level) ->
  #er_event{
     timestamp = erlang:system_time(second),
     message   = Message,
     level     = Level
    }.
