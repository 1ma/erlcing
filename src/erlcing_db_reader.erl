%%%-------------------------------------------------------------------
%%%
%%% erlcing_db_reader encapsulates read access to the Postgres DB.
%%% It is loosely modeled after a "repository" in a OOP design.
%%%
%%%-------------------------------------------------------------------
-module(erlcing_db_reader).
-export([start/5, read_stations/1, read_occupancy/2]).

start(Host, Port, Username, Password, Database) ->
  {ok, DBLink} = epgsql:connect(Host, Username, Password, [{database, Database}, {port, Port}]),
  spawn(fun() -> loop(DBLink) end).

read_stations(Pid) ->
  Pid ! {read_stations, self()},
  receive
    Stations -> Stations
  end.

read_occupancy(Pid, StationId) ->
  Pid ! {read_occupancy, self(), StationId},
  receive
    Occupancy -> Occupancy
  end.

loop(DBLink) ->
  receive
    {read_stations, Caller} ->
      spawn(fun() -> stations_sql(DBLink, Caller) end);
    {read_occupancy, Caller, StationId} ->
      spawn(fun() -> occupancy_sql(DBLink, Caller, StationId) end)
  end,
  loop(DBLink).

stations_sql(DBLink, Caller) ->
  {ok, _, Rows} = epgsql:squery(DBLink, "
    SELECT id, type, lat, lng, address
      FROM stations
     WHERE is_open IS TRUE
  ORDER BY id ASC
  "),
  Caller ! Rows.

occupancy_sql(DBLink, Caller, StationId) when StationId > 0 ->
  {ok, _, Rows} = epgsql:equery(DBLink, "
    SELECT bikes, slots, extract(EPOCH FROM observed_at) as date
      FROM occupancy
     WHERE station_id = $1
  ORDER BY observed_at ASC
  ", [StationId]),
  Caller ! Rows.
