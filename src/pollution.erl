%%%-------------------------------------------------------------------
%%% @author lukasz
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. kwi 2024 17:38
%%%-------------------------------------------------------------------
-module(pollution).
-author("Łukasz Kwinta").

%% API
-export([create_monitor/0, add_station/3, add_value/5, remove_value/4, get_one_value/4, get_station_mean/3, get_daily_mean/3, get_hourly_mean/4]).

-record(monitor, {stations}).
-record(station, {name, coordinates, measurements}).
-record(measurement, {coordinates, date_time, type, value}).

create_monitor() ->
  #monitor{stations = []}.


check_station_name_exists(Name, Monitor) ->
  lists:any(fun(Station) -> Station#station.name == Name end, Monitor#monitor.stations).

check_station_coordinates_exists(Coordinates, Monitor) ->
  lists:any(fun(Station) -> Station#station.coordinates == Coordinates end, Monitor#monitor.stations).

add_station(Name, Coordinates, Monitor) when is_tuple(Coordinates) and is_record(Monitor, monitor) ->
  case {check_station_name_exists(Name, Monitor), check_station_coordinates_exists(Coordinates, Monitor)} of
    {true, _} ->
      {error, station_name_exists};
    {_, true} ->
      {error, station_exists_at_coordinates};
    {false, false} ->
      Station = #station{name = Name, coordinates = Coordinates, measurements = []},
      Monitor#monitor{stations = [Station | Monitor#monitor.stations]}
  end;

add_station(_, _, _) ->
  {error, invalid_arguments}.

check_measurement_exists(DateTime, Type, Station) ->
  lists:any(fun (#measurement{date_time = MDateTime, type = MType}) ->
    MDateTime == DateTime andalso MType == Type end,
    Station#station.measurements).

add_value(Coordinates, DateTime, Type, Value, Monitor) when is_record(Monitor, monitor) and is_tuple(Coordinates) ->
  Station = lists:keyfind(Coordinates, #station.coordinates, Monitor#monitor.stations),
  case Station of
    false ->
      {error, station_not_found};
    _ ->
      IsAnyMeasurement = check_measurement_exists(DateTime, Type, Station),
      if
        IsAnyMeasurement ->
          {error, measurement_exists};
        true ->
          Measurement = #measurement{coordinates = Coordinates, date_time = DateTime, type = Type, value = Value},
          Station1 = Station#station{measurements = [Measurement | Station#station.measurements]},
          Monitor#monitor{stations = lists:keyreplace(Coordinates, #station.coordinates, Monitor#monitor.stations, Station1)}
      end
  end;

add_value(Name, DateTime, Type, Value, Monitor) when is_record(Monitor, monitor) ->
  Station = lists:keyfind(Name, #station.name, Monitor#monitor.stations),
  case Station of
    false ->
      {error, station_not_found};
    _ ->
      IsAnyMeasurement = check_measurement_exists(DateTime, Type, Station),

      if
        IsAnyMeasurement ->
          {error, measurement_exists};
        true ->
          Measurement = #measurement{coordinates = Station#station.coordinates, date_time = DateTime, type = Type, value = Value},
          Station1 = Station#station{measurements = [Measurement | Station#station.measurements]},
          Monitor#monitor{stations = lists:keyreplace(Name, #station.name, Monitor#monitor.stations, Station1)}
      end
  end;

add_value(_, _, _, _, _) ->
  {error, invalid_arguments}.

find_measurement(DateTime, Type, Station) ->
  MeasurementList = lists:filter(fun (#measurement{date_time = MDateTime, type = MType}) ->
    MDateTime == DateTime andalso MType == Type end,
    Station#station.measurements),
  if
    length(MeasurementList) == 0 ->
      false;
    true ->
      lists:last(MeasurementList)
  end.

remove_value(Coordinates, DateTime, Type, Monitor) when is_record(Monitor, monitor) and is_tuple(Coordinates) ->
  Station = lists:keyfind(Coordinates, #station.coordinates, Monitor#monitor.stations),
  case Station of
    false ->
      {error, station_not_found};
    _ ->
      Measurement = lists:keyfind({DateTime, Type}, #measurement.date_time, Station#station.measurements),
      case Measurement of
        false ->
          {error, measurement_not_found};
        _ ->
          Station1 = Station#station{measurements = lists:keydelete({DateTime, Type}, #measurement.date_time, Station#station.measurements)},
          Monitor#monitor{stations = lists:keyreplace(Coordinates, #station.coordinates, Monitor#monitor.stations, Station1)}
      end
  end;

remove_value(Name, DateTime, Type, Monitor) when is_record(Monitor, monitor) ->
  Station = lists:keyfind(Name, #station.name, Monitor#monitor.stations),
  case Station of
    false ->
      {error, station_not_found};
    _ ->
      Measurement = find_measurement(DateTime, Type, Station),
      case Measurement of
        false ->
          {error, measurement_not_found};
        _ ->
          Station1 = Station#station{measurements = lists:delete(Measurement, Station#station.measurements)},
          Monitor#monitor{stations = lists:keyreplace(Name, #station.name, Monitor#monitor.stations, Station1)}
      end
  end;

remove_value(_, _, _, _) ->
  {error, invalid_arguments}.

get_one_value(Coordinates, DateTime, Type, Monitor) when is_record(Monitor, monitor) and is_tuple(Coordinates) ->
  Station = lists:keyfind(Coordinates, #station.coordinates, Monitor#monitor.stations),
  case Station of
    false ->
      {error, station_not_found};
    _ ->
      Measurement = find_measurement(DateTime, Type, Station),
      case Measurement of
        false ->
          {error, measurement_not_found};
        _ ->
          Measurement#measurement.value
      end
  end;

get_one_value(Name, DateTime, Type, Monitor) when is_record(Monitor, monitor) ->
  Station = lists:keyfind(Name, #station.name, Monitor#monitor.stations),
  case Station of
    false ->
      {error, station_not_found};
    _ ->
      Measurement = find_measurement(DateTime, Type, Station),
      case Measurement of
        false ->
          {error, measurement_not_found};
        _ ->
          Measurement#measurement.value
      end
  end;

get_one_value(_, _, _, _) ->
  {error, invalid_arguments}.

get_station_mean(Coordinates, Type, Monitor) when is_record(Monitor, monitor) and is_tuple(Coordinates) ->
  Station = lists:keyfind(Coordinates, #station.coordinates, Monitor#monitor.stations),
  case Station of
    false ->
      {error, station_not_found};
    _ ->
      MeasurementList = lists:filter(fun (Measurement) -> Measurement#measurement.type == Type end, Station#station.measurements),
      if
        length(MeasurementList) == 0 ->
          {error, no_measurements};
        true ->
          lists:sum([Measurement#measurement.value || Measurement <- MeasurementList]) / length(MeasurementList)
      end
  end;

get_station_mean(Name, Type, Monitor) when is_record(Monitor, monitor) ->
  Station = lists:keyfind(Name, #station.name, Monitor#monitor.stations),
  case Station of
    false ->
      {error, station_not_found};
    _ ->
      MeasurementList = lists:filter(fun (Measurement) -> Measurement#measurement.type == Type end, Station#station.measurements),
      if
        length(MeasurementList) == 0 ->
          {error, no_measurements};
        true ->
          lists:sum([Measurement#measurement.value || Measurement <- MeasurementList]) / length(MeasurementList)
      end
  end;

get_station_mean(_, _, _) ->
  {error, invalid_arguments}.

get_measurements_date(Date, Type, Station) when is_record(Station, station) ->
  Measurements = lists:filter(fun (#measurement{date_time = {MDate, _}, type = MType}) -> MDate == Date andalso MType == Type end, Station#station.measurements),
  lists:map(fun (#measurement{value = Value}) -> Value end, Measurements).

get_daily_mean(Type, Date, #monitor{stations = Stations}) when is_list(Stations) and is_tuple(Date) ->
  MeasurementList = lists:flatmap(fun (Station) -> get_measurements_date(Date, Type, Station) end, Stations),
  if
    length(MeasurementList) == 0 ->
      {error, no_measurements};
    true ->
      lists:sum(MeasurementList) / length(MeasurementList)
  end;

get_daily_mean(A, B, C) ->
  io:format("A: ~p, B: ~p, C: ~p ~n", [A, B, C]),
  {error, invalid_arguments}.

get_measurements_hour(Time, Type, #station{measurements = Measurements}) ->
  MeasurementsTime = lists:filter(fun (#measurement{date_time = {_, MTime}, type = MType}) -> MTime == Time andalso MType == Type end, Measurements),
  lists:map(fun (#measurement{value = Value}) -> Value end, MeasurementsTime).

get_mean_from_station(Time, Type, Station) when is_record(Station, station) ->
  MeasurementList = get_measurements_hour(Time, Type, Station),
  if
    length(MeasurementList) == 0 ->
      {error, no_measurements};
    true ->
      lists:sum(MeasurementList) / length(MeasurementList)
  end.

get_hourly_mean(Coordinates, Type, Time, #monitor{stations = Stations}) when is_list(Stations) and is_tuple(Time) and is_tuple(Coordinates) ->
  get_mean_from_station(Time, Type, lists:keyfind(Coordinates, #station.coordinates, Stations));

get_hourly_mean(Name, Type, Time, #monitor{stations = Stations}) when is_list(Stations) and is_tuple(Time) ->
  get_mean_from_station(Time, Type, lists:keyfind(Name, #station.name, Stations));

get_hourly_mean(_, _, _, _) ->
  {error, invalid_arguments}.