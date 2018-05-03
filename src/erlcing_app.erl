%%%-------------------------------------------------------------------
%%%
%%% Main entry point. Run erlcing:main() to set up all processes
%%% that compose the application.
%%%
%%%-------------------------------------------------------------------
-module(erlcing_app).
-behavior(application).

-export([start/2, stop/1]).

-define(HTTP_PORT, 8080).

-define(PG_HOST, "localhost").
-define(PG_PORT, 2345).
-define(PG_USER, "postgres").
-define(PG_PASS, "postgres").
-define(PG_NAME, "erlcing_devel").

-define(COLLECTOR_SLEEP_MS, 60000).


start(_, _) ->
  register(
    erlcing_db_reader,
    erlcing_db_reader:start(?PG_HOST, ?PG_PORT, ?PG_USER, ?PG_PASS, ?PG_NAME)
  ),
  erlcing_collector:start(
    erlcing_db_writer:start(?PG_HOST, ?PG_PORT, ?PG_USER, ?PG_PASS, ?PG_NAME),
    ?COLLECTOR_SLEEP_MS
  ),
  elli:start_link([{callback, erlcing_http}, {port, ?HTTP_PORT}]).

stop(_) ->
  ok.