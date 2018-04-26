# erlcing

A port of the [Bicing project](https://github.com/1ma/bicing) to
Erlang, with the goal of learning the language and its ecosystem.


### Purpose

Just like the original project, the Erlcing application periodically pulls occupancy
data from the same source as the [official bike availability map](https://www.bicing.cat/ca/mapa-de-disponibilitat),
and stores it in a Postgres database (thus allowing the user to see the occupancy of any given station through time,
instead of just at that very same moment):

![shot](http://i.imgur.com/5ZJR7Kj.png)


### Application parts

* Frontend: A tiny, self-contained SPA with no build dependencies. Can be found at `wwwwroot/index.html`
* Nginx: Reverse proxy. In production it would handle TLS termination, static file serving, caching and rate limiting. Its config is at `resources/nginx/devel_vhost.conf`
* Backend: A simple JSON API that sits behind Nginx and a daemon that periodically pulls data from the public Bicing API. It is the code under `src/`
* Postgres: Where occupancy data is persisted. The database schema is stored at `resources/postgres/schema.sql`


### Setup

To run the app as intended you'll need the Erlang VM, rebar3, Docker CE/EE, docker-compose and GNU make available on your machine.

If your system passes these requirements, simply step into the project directory and run `make setup`. This will take care of spinning
up an Nginx and Postgres container, load the schema into the newly created database, download the dependencies and
compile the Erlang source files.

Finally, enter the Erlang shell and type `erlcing:main()`. After this, the app should be usable at `http://localhost`

```
$ make setup
cp -n docker-compose.yml.dist docker-compose.yml
docker-compose run --rm starter
Creating network "erlcing_default" with driver "bridge"
Creating erlcing_pgsql_1 ... done
Creating erlcing_nginx_1 ... done
Waiting for nginx to listen on 80...
Waiting for pgsql to listen on 5432...
sleeping
sleeping
docker-compose exec -T pgsql sh -c "psql -U postgres -d erlcing_devel < /tmp/schema.sql"
CREATE TYPE
CREATE TABLE
CREATE TABLE
CREATE TABLE
CREATE FUNCTION
CREATE TRIGGER
CREATE TRIGGER
rebar3 compile
===> Verifying dependencies...
===> Fetching elli ({pkg,<<"elli">>,<<"3.0.0">>})
===> Downloaded package, caching at /home/marcel/.cache/rebar3/hex/default/packages/elli-3.0.0.tar
===> Fetching epgsql ({pkg,<<"epgsql">>,<<"3.4.0">>})
===> Downloaded package, caching at /home/marcel/.cache/rebar3/hex/default/packages/epgsql-3.4.0.tar
===> Fetching jiffy ({pkg,<<"jiffy">>,<<"0.15.1">>})
===> Downloaded package, caching at /home/marcel/.cache/rebar3/hex/default/packages/jiffy-0.15.1.tar
===> Compiling elli
===> Compiling jiffy
Compiling c_src/decoder.c
Compiling c_src/encoder.c
Compiling c_src/jiffy.c
Compiling c_src/utf8.c
Compiling c_src/util.c
Compiling c_src/doubles.cc
Compiling c_src/objects.cc
Compiling c_src/double-conversion/bignum-dtoa.cc
Compiling c_src/double-conversion/bignum.cc
Compiling c_src/double-conversion/cached-powers.cc
Compiling c_src/double-conversion/diy-fp.cc
Compiling c_src/double-conversion/double-conversion.cc
Compiling c_src/double-conversion/fast-dtoa.cc
Compiling c_src/double-conversion/fixed-dtoa.cc
Compiling c_src/double-conversion/strtod.cc
===> Compiling epgsql
===> Compiling erlcing

$ rebar3 shell
===> Verifying dependencies...
===> Compiling erlcing
Erlang/OTP 20 [erts-9.3] [source] [64-bit] [smp:4:4] [ds:4:4:10] [async-threads:0] [kernel-poll:false]

Eshell V9.3  (abort with ^G)
1> erlcing:main().
ok
2> 
```

Caveat: Docker tries to bind the nginx and postgres containers to `localhost:80` and `localhost:2345` respectively, and
if your system has these ports already open the setup will fail. If that happens you can edit `docker-compose.yml`
and pick any other port(s) you want.


### Research topics

- [X] Manage dependencies with rebar3
- [X] Autocompile source in development environment (rebar3 auto)
- [X] Parse JSON with jiffy
- [X] HTTP client with inets/httpc
- [X] Connect to PostgreSQL with epgsql
- [X] HTTP application with ~~inets/http_server~~ ~~cowboy~~ elli
- [ ] Templating libraries? mustache.erl?
- [ ] OTPize (supervisors, gen_server)
- [ ] Figure out how to handle app parameterization (env-dependent DB credentials, etc)
- [ ] PostgreSQL connection pooling with pgapp
- [ ] PDO equivalent in Erlang? (DB interface not tied to a single engine)
- [ ] Error handling and reporting (related to OTPize)
- [ ] Code Profiling
- [ ] Live debugging (with IntelliJ + Erlang plugin)
- [ ] Dialyzer and -spec annotations
- [ ] Unit testing (eunit?)
- [ ] Functional testing (common test?)
- [ ] Code coverage
- [ ] Packaging the app for production, deploy strategies, hot swapping
- [ ] Expand rebar_cmd to accept more than one command and phase out the Makefile
