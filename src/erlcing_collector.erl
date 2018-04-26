%%%-------------------------------------------------------------------
%%%
%%% The collector leverages the bicing HTTP API to retrieve occupancy
%%% data periodically, and sends it to the erlcing_db_writer process.
%%%
%%%-------------------------------------------------------------------
-module(erlcing_collector).
-export([start/2]).

-define(BICING_API_ENDPOINT, "https://www.bicing.cat/availability_map/getJsonObject").

start(DBWriterPid, WaitTime) ->
  spawn(fun() -> loop(DBWriterPid, WaitTime) end).

loop(DBWriterPid, WaitTime) ->
  DBWriterPid ! collect(),
  timer:sleep(WaitTime),
  loop(DBWriterPid, WaitTime).

collect() ->
  {ok, {{"HTTP/1.1", 200, "OK"}, _, Body}} = httpc:request(get, {?BICING_API_ENDPOINT, []}, [], [{body_format, binary}]),
  jiffy:decode(Body, [return_maps]).
