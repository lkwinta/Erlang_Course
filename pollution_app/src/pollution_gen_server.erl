%%%-------------------------------------------------------------------
%%% @author lukasz
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(pollution_gen_server).

-behaviour(gen_server).

-export([start_link/0, add_station/2, add_value/4, remove_value/3, get_one_value/3, get_station_mean/2, get_daily_mean/2, get_hourly_mean/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% User interface
%%%===================================================================

add_station(Name, Coordinates)          -> gen_server:cast(?MODULE, {add_station, Name, Coordinates}).
add_value(Name, DateTime, Type, Value)  -> gen_server:cast(?MODULE, {add_value, Name, DateTime, Type, Value}).
remove_value(Name, DateTime, Type)      -> gen_server:cast(?MODULE, {remove_value, Name, DateTime, Type}).

get_one_value(Name, DateTime, Type)     -> gen_server:call(?MODULE, {get_one_value, Name, DateTime, Type}).
get_station_mean(Name, Type)            -> gen_server:call(?MODULE, {get_station_mean, Name, Type}).
get_daily_mean(DateTime, Type)          -> gen_server:call(?MODULE, {get_daily_mean, DateTime, Type}).
get_hourly_mean(Name, Type, DateTime)   -> gen_server:call(?MODULE, {get_hourly_mean, Name, Type, DateTime}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
  Monitor = pollution:create_monitor(),
  {ok, Monitor}.

handle_cast(Request, Monitor) ->
  ResultMonitor = case Request of
                    {add_station, Name, Coordinates} -> pollution:add_station(Name, Coordinates, Monitor);
                    {add_value, Name, DateTime, Type, Value} -> pollution:add_value(Name, DateTime, Type, Value, Monitor);
                    {remove_value, Name, DateTime, Type} -> pollution:remove_value(Name, DateTime, Type, Monitor)
                  end,
  case ResultMonitor of
    {error, Reason} ->
      io:format("Error: ~p ~n", [Reason]),
      {noreply, Monitor};
    _ -> {noreply, ResultMonitor}
  end.

handle_call(Request, _From, Monitor) ->
  Result = case Request of
             {get_one_value, Name, DateTime, Type} -> pollution:get_one_value(Name, DateTime, Type, Monitor);
             {get_station_mean, Name, Type} -> pollution:get_station_mean(Name, Type, Monitor);
             {get_daily_mean, DateTime, Type} -> pollution:get_daily_mean(Type, DateTime, Monitor);
             {get_hourly_mean, Name, Type, DateTime} -> pollution:get_hourly_mean(Name, Type, DateTime, Monitor)
  end,
  case Result of
      {error, Reason} -> {reply, {error, Reason}, Monitor};
      _ -> {reply, Result, Monitor}
  end.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
