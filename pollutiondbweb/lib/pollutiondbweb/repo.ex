defmodule Pollutiondbweb.Repo do
  use Ecto.Repo,
    otp_app: :pollutiondbweb,
    adapter: Ecto.Adapters.SQLite3
end
