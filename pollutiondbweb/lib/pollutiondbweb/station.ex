require Ecto.Query

defmodule Pollutiondbweb.Station do
  use Ecto.Schema

  schema "stations" do
    field :name, :string
    field :lon, :float
    field :lat, :float

    has_many :readings, Pollutiondbweb.Reading
  end

  def add(station) do
    Pollutiondbweb.Repo.insert(station)
  end

  def get_all() do
    Pollutiondbweb.Repo.all(Pollutiondbweb.Station)
  end

  def get_by_id(id) do
    Pollutiondbweb.Repo.get(Pollutiondbweb.Station, id)
  end

  def find_by_name(name) do
    Pollutiondbweb.Repo.all(Ecto.Query.where(Pollutiondbweb.Station, name: ^name))
  end

  def find_by_name_like(name) do
    Ecto.Query.from(s in Pollutiondbweb.Station,
        where: like(s.name, ^"%#{name}%"))
      |> Pollutiondbweb.Repo.all
  end

  def find_by_location(lon, lat) do
    Ecto.Query.from(s in Pollutiondbweb.Station,
        where: s.lon == ^lon,
        where: s.lat == ^lat)
      |> Pollutiondbweb.Repo.all
  end

  def find_by_location_range(lon_min, lon_max, lat_min, lat_max) do
    Ecto.Query.from(s in Pollutiondbweb.Station,
          where: s.lon >= ^lon_min,
          where: s.lon <= ^lon_max,
          where: s.lat >= ^lat_min,
          where: s.lat <= ^lat_max)
      |> Pollutiondbweb.Repo.all
  end

  def update_name(station, newname) do
    station
      |> changeset(%{name: newname})
      |> Pollutiondbweb.Repo.update
  end

  def add(name, lon, lat) do
    %Pollutiondbweb.Station{}
      |> changeset(%{name: name, lon: lon, lat: lat})
      |> Pollutiondbweb.Repo.insert
  end

  defp changeset(station, changesmap) do
    station
      |> Ecto.Changeset.cast(changesmap, [:name, :lon, :lat])
      |> Ecto.Changeset.validate_required([:name, :lon, :lat])
      |> Ecto.Changeset.validate_number(:lon, greater_than: -180, less_than: 180)
      |> Ecto.Changeset.validate_number(:lat, greater_than: -90, less_than: 90)
  end
end
