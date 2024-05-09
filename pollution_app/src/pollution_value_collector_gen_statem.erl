%%%-------------------------------------------------------------------
%%% @author lukasz
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. maj 2024 23:08
%%%-------------------------------------------------------------------
-module(pollution_value_collector_gen_statem).
-author("lukasz").

-behaviour(gen_statem).

%% API
-export([start_link/0, stop/0, set_station/1, add_value/3, store_data/0]).

%% gen_statem callbacks
-export([init/1, terminate/3, callback_mode/0, station_unset/3, waiting_for_values/3]).

-define(SERVER, ?MODULE).

-record(state, {station = undefined, readings = []}).
-record(reading, {value, date_time, type}).

%%%===================================================================
%%% User interface
%%%===================================================================
stop() ->
  gen_statem:stop(pollution_value_collector_gen_statem).

set_station(NameCoordinates) ->     gen_statem:cast(?SERVER, {set_station, NameCoordinates}).
add_value(Value, DateTime, Type) -> gen_statem:cast(?SERVER, {add_value, Value, DateTime, Type}).
store_data() ->                     gen_statem:cast(?SERVER, {store_data}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================
start_link() ->
  gen_statem:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
  {ok, station_unset, #state{}}.

station_unset(_EventType, {set_station, NameCoordinates}, _) ->
  {next_state, waiting_for_values, #state{station = NameCoordinates, readings = []}}.

waiting_for_values(_EventType, {add_value, Value, DateTime, Type}, #state{station=Station, readings=Readings}) ->
  {keep_state, #state{station=Station, readings=[#reading{value=Value, date_time=DateTime, type=Type} | Readings]}};
waiting_for_values(_EventType, {store_data}, #state{station=NameCoordinates, readings=Readings}) ->
  add_values(NameCoordinates, Readings),
  {next_state, station_unset, #state{}}.

callback_mode() ->
  state_functions.

terminate(Reason, StateName, State) ->
  io:format("Error: ~p. ~n", [Reason]),
  io:format("StateName: ~p. ~n", [StateName]),
  io:format("State: ~w. ~n", [State]),
  ok.

add_values(_, []) ->
  ok;
add_values(NameCoordinates, [#reading{value=Value, date_time=DateTime, type=Type} | T]) ->
  pollution_gen_server:add_value(NameCoordinates, DateTime, Value, Type),
  add_values(NameCoordinates, T).
