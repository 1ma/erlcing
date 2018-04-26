%%%-------------------------------------------------------------------
%%%
%%% erlcing_db_writer works in tandem with erlcing_collector to
%%% persist to Postgres the occupancy data it receives.
%%%
%%%-------------------------------------------------------------------
-module(erlcing_db_writer).
-export([start/5]).

start(Host, Port, Username, Password, Database) ->
  {ok, DBLink} = epgsql:connect(Host, Username, Password, [{database, Database}, {port, Port}]),
  spawn(fun() -> loop(DBLink) end).

loop(DBLink) ->
  receive
    Observation ->
      save(DBLink, erlang:timestamp(), Observation),
      loop(DBLink)
  end.

save(_, _, []) -> ok;
save(DBLink, Timestamp, [H | T]) ->
  ok = save_station(DBLink, H),
  ok = save_occupancy(DBLink, Timestamp, H),
  save(DBLink, Timestamp, T).

save_station(DBLink, #{
  <<"id">> := Id,
  <<"status">> := Status,
  <<"stationType">> := StationType,
  <<"lat">> := Lat,
  <<"lon">> := Lon,
  <<"address">> := Address
}) ->
  IsOpen = case Status of
    <<"OPN">> -> true;
    <<"CLS">> -> false
  end,
  Type = case StationType of
    <<"BIKE">> -> "regular";
    <<"ELECTRIC_BIKE">> -> "electric"
  end,
  epgsql:equery(DBLink, "
    INSERT INTO stations (id, is_open, type, lat, lng, address)
    VALUES ($1, $2, $3, $4, $5, $6)
    ON CONFLICT (id) DO NOTHING
  ", [binary_to_integer(Id), IsOpen, Type, binary_to_float(Lat), binary_to_float(Lon), binary_to_list(Address)]),
  ok.

save_occupancy(DBLink, Timestamp, #{
  <<"id">> := Id,
  <<"bikes">> := Bikes,
  <<"slots">> := Slots
}) ->
  epgsql:equery(DBLink, "
    INSERT INTO occupancy (station_id, bikes, slots, observed_at)
    VALUES ($1, $2, $3, $4)
  ", [binary_to_integer(Id), binary_to_integer(Bikes), binary_to_integer(Slots), Timestamp]),
  ok.
