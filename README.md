eraven
=====

[![Build Status](https://travis-ci.org/bottleneko/eraven.svg?branch=master)](https://travis-ci.org/bottleneko/eraven)

Erlang client for Sentry.

## Features

* Zero third-party deps
* Support many kinds of [Sentry Interfaces](https://docs.sentry.io/development/sdk-dev/interfaces/)
* Flexible logging interfaces
* Use as new Erlang/OTP 21.0 logger handler

## Examples

Here's example of using eraven in the Erlang shell:

```erlang
1> logger:add_handler(
    eraven,
    er_logger_handler,
    #{config => #{
      dsn => "http://9f293de25b2c4a74b09ae731ba6aac58@localhost:9000/1"
    }}).
2> logger:error("Test error", []).
```
Dsn string is located in the `Client Keys (DSN)` section of your `Project Settings` in Sentry.

## Contexts

eraven supports various contexts also known as Sentry interfaces.

### [Sentry User Context](https://docs.sentry.io/development/sdk-dev/interfaces/#user-interface)

```erlang
1> set_user_context(
    #{id         => <<"test_id">>,
      username   => <<"some_user">>,
      email      => <<"user@example.com">>,
      ip_address => {8,8,8,8}
     }).
```

User context being installed for a process, all logging events captured by eraven be have a user context.

### [Sentry Request Context](https://docs.sentry.io/development/sdk-dev/interfaces/#http-interface)

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

### [Sentry Environment Context](https://docs.sentry.io/development/sdk-dev/attributes/#required-attributes)

```erlang
1> set_environment_context(eraven, <<"test_server">>, <<"test_enviroment">>, <<"v0.1.0">>),
```

Request context being installed for an eraven log handler, all logging events captured by this handler be have a enviroment context.

### [Sentry Tags](https://docs.sentry.io/development/sdk-dev/attributes/#optional-attributes)

Eraven supports two types of tags. Is process and event tags. Event tags be used from process metadata by key setted in config `event_tags_key`.

```erlang
1> logger:add_handler(
    eraven,
    er_logger_handler,
    #{config => #{dsn                  => "http://9f293de25b2c4a74b09ae731ba6aac58@localhost:9000/1",
                  json_encode_function => fun jsx:encode/1,
                  event_tags_key       => event_tags,
                  event_extra_key      => event_extra,
                  fingerprint_key      => fingerprint
                 }}).
2> eraven:set_process_tags(#{test_tag => tag}).
3> logger:error("Test error", [], #{event_tags => #{other_test_tag => other_tag}}).
```

### [Sentry Extra](https://docs.sentry.io/development/sdk-dev/attributes/#optional-attributes)

In same way you can configure extra: from event metadata and from process metadata by key setted in config `event_extra_key`

```erlang
1> logger:add_handler(
    eraven,
    er_logger_handler,
    #{config => #{dsn                  => "http://9f293de25b2c4a74b09ae731ba6aac58@localhost:9000/1",
                  json_encode_function => fun jsx:encode/1,
                  event_tags_key       => event_tags,
                  event_extra_key      => event_extra,
                  fingerprint_key      => fingerprint
                 }}).
2> eraven:set_process_tags(#{extra => extra}).
3> logger:error("Test error", [], #{event_extra => #{other_extra => other_extra}}).
```

### [Sentry Fingerprint](https://docs.sentry.io/data-management/rollups)

You can configure fingerprint from event metadata by key setting in config `event_extra_key`

```erlang
1> logger:add_handler(
    eraven,
    er_logger_handler,
    #{config => #{dsn                  => "http://9f293de25b2c4a74b09ae731ba6aac58@localhost:9000/1",
                  json_encode_function => fun jsx:encode/1,
                  event_tags_key       => event_tags,
                  event_extra_key      => event_extra,
                  fingerprint_key      => fingerprint
                 }}).
2> logger:error("Test error", [], #{fingerprint => [<<"default">>]}}).
```

## Development

Installation

    $ make

Clean-up

    $ make clean

Sentry available on http://localhost:9000 by account admin@example.com and password admin

## Use

Rebar3

    {deps, [eraven]}.

## Roadmap

- [x] Writing test for 100% coverage
- [x] Fix all dialyzer errors
- [ ] Support Breadcrumbs
- [ ] Adding [Elvis](https://github.com/inaka/elvis)
- [ ] Usage in production
- [ ] [Unified API](https://docs.sentry.io/development/sdk-dev/unified-api/)
- [ ] Benchmarks
- [ ] Usage instructions for Erlang.mk
