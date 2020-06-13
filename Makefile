.PHONY: setup

setup:
	docker-compose run --rm starter
	docker-compose exec -T postgres sh -c "psql -U postgres -d erlcing_devel < /tmp/schema.sql"
	rebar3 compile
