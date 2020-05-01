-module(er_config).

-type t() :: #{dsn                  := er_dsn:t()     | undefined,
               event_extra_key      := atom(),
               event_tags_key       := atom(),
               fingerprint_key      := atom(),
               json_encode_function := fun((map()) -> binary()),
               report_depth         := non_neg_integer(),
               report_chars_limit   := non_neg_integer(),
               environment_context  := er_context:t() | undefined
              }.

-export_type([t/0]).

-export([new/1, default/0]).

%%%===================================================================
%%% API
%%%===================================================================

-spec default() -> t().
default() ->
  #{dsn                  => undefined,
    event_extra_key      => event_extra,
    event_tags_key       => event_tags,
    fingerprint_key      => fingerprint,
    json_encode_function => fun jsx:encode/1,
    report_depth         => 20,
    report_chars_limit   => 4096,
    environment_context  => undefined
   }.

-spec new(RawConfig) -> {ok, t()} | {error, Reason} when
    RawConfig :: map(),
    Reason    :: binary().
new(#{dsn := _Dsn} = RawConfig0) ->
  ConfigKeys = maps:keys(default()),
  RawConfig1 = maps:with(ConfigKeys, RawConfig0),
  RawConfig2 = maps:merge(default(), RawConfig1),
  parse_dsn(RawConfig2);
new(_RawConfig) ->
  {error, <<"Missing required property `dsn`">>}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec parse_dsn(RawConfig) -> {ok, t()} | {error, Reason} when
    RawConfig :: map(),
    Reason    :: binary().
parse_dsn(#{dsn := DsnString} = RawConfig) ->
  case er_dsn:new(DsnString) of
    {ok, Dsn} ->
      {ok, RawConfig#{dsn => Dsn}};
    {error, _Reason} = Error ->
      Error
  end.
