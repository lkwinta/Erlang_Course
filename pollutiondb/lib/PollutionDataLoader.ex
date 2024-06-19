require Pollutiondb.Reading
require Pollutiondb.Station

defmodule PollutionDataLoader do
  def load_data() do
    load_file()
      |> Enum.filter(&(String.length(&1) > 0))
      |> Enum.map(&parse_line/1)
  end

  def upload_all_data_from_file() do
    readings = load_data()
    upload_stations(readings)
    upload_readings(readings)
  end

  def upload_stations(readings) do
    readings
      |> identify_stations()
      |> Enum.map(&Pollutiondb.Station.add("#{&1.id} #{&1.name}", &1.location.lon, &1.location.lat))
  end

  def upload_readings(readings) do
    readings
      |> Enum.map(&add_reading/1)
  end

  defp load_file() do
    File.read!(__DIR__ <> "/../../AirlyData-ALL-50k.csv")
      |> String.split(["\n", "\r\n"])
  end

  defp identify_stations(readings) do
    readings
      |> Enum.uniq_by(& &1.id)
      |> Enum.map(&%{:id => &1.id, :name => &1.name, :location => &1.location})
  end

  defp add_reading(reading) do
    Pollutiondb.Station.find_by_name("#{reading.id} #{reading.name}")
      |> Enum.at(0)
      |> Pollutiondb.Reading.add(reading.date_time.date, reading.date_time.time, reading.type, reading.value)
  end

  defp parse_line(line) do
    [date_time, type, value, id, name, location] = line |> String.split(";")

    %{
      :date_time => parse_date(date_time),
      :type => type,
      :value => String.to_float(value),
      :id => String.to_integer(id),
      :name => name,
      :location => parse_location(location)
    }
  end

  defp parse_date(date) do
    [date, time] = date |> String.slice(0..18) |> String.split("T")

    %{
      :date => date
        |> String.split("-")
        |> Enum.map(&String.to_integer/1)
        |> List.to_tuple()
        |> Date.from_erl!(),
      :time => time
        |> String.split(":")
        |> Enum.map(&String.to_integer/1)
        |> List.to_tuple()
        |> Time.from_erl!()
    }
  end

  defp parse_location(location) do
    [lon, lat] = location |> String.split(",") |> Enum.map(&String.to_float/1)

    %{
      :lon => lon,
      :lat => lat
    }
  end
end