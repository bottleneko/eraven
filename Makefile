.DEFAULT_GOAL := all

.NOTPARALLEL:

DOCKER := docker
COMPOSE := docker-compose

EXEC := $(COMPOSE) exec workspace

REBAR := $(EXEC) rebar3

.PHONY: all
all: .env workspace-build install-sentry workspace-up shell

### ==================================================================
### Local environment
### ==================================================================

.PHONY: workspace
workspace:
	$(EXEC) bash

.PHONY: workspace-build
workspace-build:
	$(COMPOSE) build

.PHONY: workspace-build
workspace-up:
	$(COMPOSE) up --detach --remove-orphans

.PHONY: workspace-down
workspace-down:
	$(COMPOSE) down --remove-orphans

.env:
	cp .env.example .env

.PHONY: install-sentry
install-sentry:
	$(COMPOSE) run --rm sentry upgrade --noinput &>/dev/null | true
	$(COMPOSE) run --rm sentry createuser --email admin@example.com --password admin --superuser &>/dev/null | true

.PHONY: clean
clean:
	$(REBAR) clean
	$(COMPOSE) down --remove-orphans --volumes --rmi local
	rm .env

### ==================================================================
### Development
### ==================================================================

.PHONY: compile
compile:
	$(REBAR) compile

.PHONY: shell
shell:
	$(REBAR) shell

.PHONY: coverage
coverage:
	$(REBAR) ct --cover
	$(REBAR) eunit --cover
	$(REBAR) cover -v
