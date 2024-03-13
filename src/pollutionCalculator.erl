%%%-------------------------------------------------------------------
%%% @author Łukasz Kwinta
%%% @copyright (C) 2024, <COMPANY>
%%% @doc module allowing to calculate pollution level from meter stations
%%%
%%% @end
%%% Created : 29. lut 2024 12:29
%%%-------------------------------------------------------------------
-module(pollutionCalculator).
-author("Łukasz Kwinta").

%% API
-export([get_readings/0, number_of_readings/2, calculate_max/2, calculate_mean/2]).

random_float(A, B) ->
  A + (B - A) * rand:uniform().

get_reading(StationName, Date) ->
  {
    reading,
    StationName,
    Date,
    time(),
    [
      {temperature, random_float(-10, 30)},
      {humidity, random_float(0.5, 0.9)},
      {pm10, random_float(0, 100)},
      {pm25, random_float(0, 100)},
      {pm1, random_float(0, 100)}
    ]
  }.

get_readings() ->
  [
    get_reading("Kraków", date()),
    get_reading("Warszawa", date()),
    get_reading("Szczecin", {2024, 2, 28}),
    get_reading("Gdańsk", {2024, 2, 27}),
    get_reading("Wrocław", {2024, 2, 26})
  ].

number_of_readings(Readings, _) when erlang:element(1, Readings) /= reading ->
  io:format("Wrong Readings tuple~n"),
  {error, wrong_readings_tuple};
number_of_readings([H | T], Date) when erlang:element(3, H) == Date ->
  1 + number_of_readings(T, Date);
number_of_readings([_ | T], Date) -> number_of_readings(T, Date);
number_of_readings([], _) -> 0.

get_sensor_value(Reading, Type)  ->
  proplists:get_value(Type, erlang:element(5, Reading)).

calculate_max_tail(_, _, undefined) ->
  io:format("Given reading type does not exists in each reading~n"),
  {error, non_existing_reading_type};
calculate_max_tail([H | T], Type, Accum) ->
  calculate_max_tail(T, Type, max(get_sensor_value(H, Type), Accum));
calculate_max_tail([], _, Accum) ->
  Accum.

calculate_max(Readings, _) when erlang:element(1, Readings) /= reading ->
  io:format("Wrong Readings tuple~n"),
  {error, wrong_readings_tuple};
calculate_max([H | T], Type) ->
  calculate_max_tail(T, Type, get_sensor_value(H, Type)).

calculate_mean_tail(_, _, undefined, _) ->
  io:format("Given reading type does not exists in each reading~n"),
  {error, non_existing_reading_type};
calculate_mean_tail([H | T], Type, Accum, Count) ->
  calculate_mean_tail(T, Type, Accum + get_sensor_value(H, Type), Count + 1);
calculate_mean_tail([], _, Accum, Count) ->
  Accum / Count.

calculate_mean(Readings, _) when erlang:element(1, Readings) /= reading ->
  io:format("Wrong Readings tuple~n"),
  {error, wrong_readings_tuple};
calculate_mean([H | T], Type) ->
  calculate_mean_tail(T, Type, get_sensor_value(H, Type), 1).
