install-sentry:
	docker volume create --name=sentry-data
	docker volume create --name=sentry-postgres
	test -f .env && touch .env
	docker-compose build
	echo "SENTRY_SECRET_KEY=z7pq%@lw6ip_ef0+%=m=jr9w5&-q8k0gdds1@!sbb(3(^j6gcl" >> .env
	docker-compose run --rm web upgrade
	docker-compose up -d

remove-sentry:
	docker-compose down
	docker volume rm sentry-data
	docker volume rm sentry-postgres

coverage:
	rebar3 ct --cover
	rebar3 eunit --cover
	rebar3 cover -v
