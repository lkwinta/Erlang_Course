require Ecto.Query

defmodule Pollutiondb.Reading do
  use Ecto.Schema

  schema "readings" do
    field :value, :float
    field :type, :string
    field :date, :date
    field :time, :time

    belongs_to :station, Pollutiondb.Station
  end

  def add_now(station, type, value) do
    reading = %Pollutiondb.Reading{
      type: type,
      value: value,
      date: Date.utc_today,
      time: Time.truncate(Time.utc_now, :second)}

    Ecto.build_assoc(station, :readings, reading) |> Pollutiondb.Repo.insert
  end

  def find_by_date(date) do
    Ecto.Query.from(r in Pollutiondb.Reading,
        where: r.date == ^date)
      |> Pollutiondb.Repo.all
  end
end