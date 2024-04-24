%%%-------------------------------------------------------------------
%%% @author lukasz
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. kwi 2024 12:17
%%%-------------------------------------------------------------------
-module(sensor_dist).
-author("lukasz").

%% API
-export([get_random_locations/1, find_closest/2, find_for_person/3, find_closest_parallel/2]).

get_random_locations(Number) ->
  [{rand:uniform(10000), rand:uniform(10000)} || _ <- lists:seq(1, Number)].

dist({X1, Y1}, {X2, Y2}) ->
  math:sqrt(math:pow(X1 - X2, 2) + math:pow(Y1 - Y2, 2)).

find_for_person(PersonLocation, SensorsLocations) ->
  Dist = [{dist(PersonLocation, SensorLocation), PersonLocation, SensorLocation} || SensorLocation <- SensorsLocations],
  lists:min(Dist).

find_closest(PeoplesLocations, SensorsLocations) ->
  Dist = [find_for_person(PersonLocation, SensorsLocations) || PersonLocation <- PeoplesLocations],
  lists:min(Dist).

find_for_person(PersonLocation, SensorsLocations, ParentPID) ->
  Dist = [{dist(PersonLocation, SensorLocation), PersonLocation, SensorLocation} || SensorLocation <- SensorsLocations],
  ParentPID ! lists:min(Dist).

find_closest_parallel(PeoplesLocations, SensorsLocations) ->
  [spawn(?MODULE, find_for_person, [PersonLocation, SensorsLocations, self()]) || PersonLocation <- PeoplesLocations],
  Dists = [receive M -> M end || _ <- PeoplesLocations],
  lists:min(Dists).