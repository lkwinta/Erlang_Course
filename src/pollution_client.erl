%%%-------------------------------------------------------------------
%%% @author lukasz
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. kwi 2024 23:12
%%%-------------------------------------------------------------------
-module(pollution_client).
-author("lukasz").

%% API
-export([add_station/2, add_value/4, remove_value/3, get_one_value/3, get_station_mean/2, get_daily_mean/2, get_hourly_mean/3]).

call(Message) ->
  pollution_server ! {request, self(), Message},
  receive
    {reply, Result} ->
      Result
  end.

add_station(Name, Coordinates) -> call({add_station, Name, Coordinates}).
add_value(Name, DateTime, Type, Value) -> call({add_value, Name, DateTime, Type, Value}).
remove_value(Name, DateTime, Type) -> call({remove_value, Name, DateTime, Type}).
get_one_value(Name, DateTime, Type) -> call({get_one_value, Name, DateTime, Type}).
get_station_mean(Name, Type) -> call({get_station_mean, Name, Type}).
get_daily_mean(DateTime, Type) -> call({get_daily_mean, DateTime, Type}).
get_hourly_mean(DateTime, Type, Hour) -> call({get_hourly_mean, DateTime, Type, Hour}).