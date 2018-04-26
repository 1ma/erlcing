.PHONY: setup

setup:
	cp -n docker-compose.yml.dist docker-compose.yml
	docker-compose run --rm starter
	docker-compose exec -T pgsql sh -c "psql -U postgres -d erlcing_devel < /tmp/schema.sql"
	rebar3 compile
