defmodule PollutiondbwebWeb.StationLive do
  use PollutiondbwebWeb, :live_view

  alias Pollutiondbweb.Station

  def mount(_params, _session, socket) do
    socket = assign(socket, stations: Station.get_all(), name: "", lat: "", lon: "")
    {:ok, socket}
  end

  def handle_event("insert", %{"name" => name, "lat" => lat, "lon" => lon}, socket) do
    Station.add(%Station{name: name, lat: to_float(lat, 0.0), lon: to_float(lon, 0.0)})
    socket = assign(socket, stations: Station.get_all(), name: name, lat: lat, lon: lon)
    {:noreply, socket}
  end

  def handle_event("search", %{"name" => station_name}, socket) do
    socket = case station_name do
      "" -> assign(socket, stations: Station.get_all())
      _ -> assign(socket, stations: Station.find_by_name_like(station_name))
    end
    {:noreply, socket}
  end

  def to_float(value, default) do
    try do
      String.to_float(value)
    rescue
      ArgumentError -> default
    end
  end

  def render(assigns) do
    ~H"""
    Create new station
    <form phx-submit="insert">
      Name: <input type="text" name="name" value={@name} /><br/>
      Lat: <input type="number" name="lat" step="0.1" value={@lat} /><br/>
      Lon: <input type="number" name="lon" step="0.1" value={@lon} /><br/>
      <input type="submit" />
    </form>
    Find stations
    <form phx-change="search">
      Station change: <input type="text" name="name" value={@name}/>
    </form>
    <table>
      <tr>
        <th>Name</th><th>Longitude</th><th>Latitude</th>
      </tr>
      <%= for station <- @stations do %>
        <tr>
          <td><%= station.name %></td>
          <td><%= station.lon %></td>
          <td><%= station.lat %></td>
        </tr>
      <% end %>
    </table>
    """
  end
end
