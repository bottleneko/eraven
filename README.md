eraven
=====

[![Build Status](https://travis-ci.org/bottleneko/eraven.svg?branch=master)](https://travis-ci.org/bottleneko/eraven)

Erlang client for Sentry.

Features
-----

* Zero third-party deps
* Support many kinds of [Sentry Interfaces](https://docs.sentry.io/development/sdk-dev/interfaces/)
* Flexible logging interfaces
* Use as new Erlang/OTP 21.0 logger handler

Examples
-----

Here's example of using eraven in the Erlang shell:

```erlang
1> {ok, Dsn} = er_dsn:new("http://9f293de25b2c4a74b09ae731ba6aac58@localhost:9000/1").
2> logger:add_handler(
    eraven,
    er_logger_handler,
    #{config => #{dsn                  => Dsn,
                  json_encode_function => fun jsx:encode/1,
                  event_tags_key       => event_tags,
                  event_extra_key      => event_extra
                 }}).
3> logger:error("Test error", []).
```

Contexts
-----

eraven supports various contexts also known as Sentry interfaces.

It's user context:

```erlang
1> set_user_context(
    #{id         => <<"test_id">>,
      username   => <<"some_user">>,
      email      => <<"user@example.com">>,
      ip_address => {8,8,8,8}
     }).
```

User context being installed for a process, all logging events captured by eraven be have a user context.

Request context:

```erlang
1> set_request_context(
    'POST',
    <<"https://localhost:9000/api/test">>,
    #{<<"test header">> => <<"test header value">>,
      <<"Content-Type">> => <<"application/json">>
     },
    #{<<"TEST_ENV">> => <<"TEST_ENV_VALUE">>},
    #{<<"body_key">> => <<"body_value">>}).
```

Request context being installed for a process, all logging events captured by eraven be have an request context.

Enviroment context:

```erlang
1> set_environment_context(eraven, <<"test_server">>, <<"test_enviroment">>, <<"v0.1.0">>),
```

Request context being installed for an eraven log handler, all logging events captured by this handler be have a enviroment context.

Tags:

Eraven supports two types of tags. Is process and event tags. Event tags be used from process metadata by key setted in config `event_tags_key`.

```erlang
1> {ok, Dsn} = er_dsn:new("http://9f293de25b2c4a74b09ae731ba6aac58@localhost:9000/1").
2> logger:add_handler(
    eraven,
    er_logger_handler,
    #{config => #{dsn                  => Dsn,
                  json_encode_function => fun jsx:encode/1,
                  event_tags_key       => event_tags,
                  event_extra_key      => event_extra,
                  fingerprint_key      => fingerprint
                 }}).
3> eraven:set_process_tags(#{test_tag => tag}).
4> logger:error("Test error", [], #{event_tags => #{other_test_tag => other_tag}}).
```

In same way you can configure extra: from event metadata and from process metadata by key setted in config `event_extra_key`

```erlang
1> {ok, Dsn} = er_dsn:new("http://9f293de25b2c4a74b09ae731ba6aac58@localhost:9000/1").
2> logger:add_handler(
    eraven,
    er_logger_handler,
    #{config => #{dsn                  => Dsn,
                  json_encode_function => fun jsx:encode/1,
                  event_tags_key       => event_tags,
                  event_extra_key      => event_extra,
                  fingerprint_key      => fingerprint
                 }}).
3> eraven:set_process_tags(#{extra => extra}).
4> logger:error("Test error", [], #{event_tags => #{other_extra => other_extra}}).
```

### [Sentry Fingerprint](https://docs.sentry.io/data-management/rollups)

You can configure fingerprint from event metadata by key setting in config `event_extra_key`

```erlang
1> {ok, Dsn} = er_dsn:new("http://9f293de25b2c4a74b09ae731ba6aac58@localhost:9000/1").
2> logger:add_handler(
    eraven,
    er_logger_handler,
    #{config => #{dsn                  => Dsn,
                  json_encode_function => fun jsx:encode/1,
                  event_tags_key       => event_tags,
                  event_extra_key      => event_extra,
                  fingerprint_key      => fingerprint
                 }}).
3> logger:error("Test error", [], #{fingerprint => [<<"default">>]}}).
```

Development
-----

Installation

    $ make install-sentry

Clean-up

    $ make remove-sentry

Use
-----

    {deps, [eraven]}.
