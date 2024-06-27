require Ecto.Query

defmodule Pollutiondbweb.Reading do
  use Ecto.Schema

  schema "readings" do
    field :value, :float
    field :type, :string
    field :date, :date
    field :time, :time

    belongs_to :station, Pollutiondbweb.Station
  end

  def add(station, date, time, type, value) do
    reading = %Pollutiondbweb.Reading{
      type: type,
      value: value,
      date: date,
      time: time}

    Ecto.build_assoc(station, :readings, reading) |> Pollutiondbweb.Repo.insert
  end

  def add_now(station, type, value) do
    add(
      station,
      Date.utc_today,
      Time.truncate(Time.utc_now, :second),
      type,
      value)
  end

  def find_by_date(date) do
    Ecto.Query.from(r in Pollutiondbweb.Reading,
        where: r.date == ^date)
      |> Pollutiondbweb.Repo.all
  end
end
