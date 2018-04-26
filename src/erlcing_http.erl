%%%-------------------------------------------------------------------
%%%
%%% erlcing_http exposes the HTTP API that the frontend relies on
%%% in order to display the maps.
%%%
%%%-------------------------------------------------------------------
-module(erlcing_http).
-export([handle/2, handle_event/3]).

-include_lib("elli/include/elli.hrl").
-behaviour(elli_handler).

handle(Req, _Args) -> handle(Req#req.method, elli_request:path(Req), Req).

handle('GET', [<<"hello">>, <<"world">>], _Req) ->
  {ok, [], <<"ola ke ase\n">>};

handle('GET', [<<"hello">>, Name], _Req) ->
  {ok, [], <<"Hello ", Name/binary, "\n">>};

handle('GET', [<<"stations">>], _Req) ->
  {
    ok,
    [{<<"Content-Type">>, <<"application/json">>}],
    jiffy:encode(
      lists:map(
        fun({Id, Type, Lat, Lon, Address}) -> {[
          {id, Id},
          {type, Type},
          {lat, Lat},
          {lng, Lon},
          {address, Address}
        ]} end,
        erlcing_db_reader:read_stations(whereis(erlcing_db_reader))
      )
    )
  };

handle('GET', [<<"stations">>, StationId], _Req) ->
  {
    ok,
    [{<<"Content-Type">>, <<"application/json">>}],
    jiffy:encode(
      lists:map(
        fun({Bikes, Slots, Date}) -> {[
          {bikes, Bikes},
          {slots, Slots},
          {date, integer_to_binary(round(Date))}
        ]} end,
        erlcing_db_reader:read_occupancy(
          whereis(erlcing_db_reader), binary_to_integer(StationId)
        )
      )
    )
  };

handle(_, _, _Req) ->
  {404, [{<<"Server">>, <<"Erlang/20.3">>}], <<"Nat Faunt\n">>}.

handle_event(_Event, _Data, _Args) -> ok.
