%%%-------------------------------------------------------------------
%%% @author lukasz
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. kwi 2024 22:41
%%%-------------------------------------------------------------------
-module(pollution_server).
-author("lukasz").

%% API
-export([start/0, stop/0]).

start() ->
  register(pollution_server, spawn(fun () -> init() end)),
  ok.

stop() ->
  pollution_server ! stop.

init() ->
  Monitor = pollution:create_monitor(),
  loop(Monitor),
  ok.

loop(Monitor) ->
  receive
    stop ->
      io:format("Stopping server ~n"),
      ok;
    {request, Client, Message} ->
      Result = case Message of
                  {add_station, Name, Coordinates} -> pollution:add_station(Name, Coordinates, Monitor);
                  {add_value, Name, DateTime, Type, Value} -> pollution:add_value(Name, DateTime, Type, Value, Monitor);
                  {remove_value, Name, DateTime, Type} -> pollution:remove_value(Name, DateTime, Type, Monitor);
                  {get_one_value, Name, DateTime, Type} -> pollution:get_one_value(Name, DateTime, Type, Monitor);
                  {get_station_mean, Name, Type} -> pollution:get_station_mean(Name, Type, Monitor);
                  {get_daily_mean, DateTime, Type} -> pollution:get_daily_mean(DateTime, Type, Monitor);
                  {get_hourly_mean, DateTime, Type, Hour} -> pollution:get_hourly_mean(DateTime, Type, Hour, Monitor)
               end,
      Client ! {reply, Result},
      loop(Result)
  end.