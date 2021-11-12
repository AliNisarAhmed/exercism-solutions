defmodule CaptainsLog do
  @planetary_classes ["D", "H", "J", "K", "L", "M", "N", "R", "T", "Y"]

  def random_planet_class() do
    Enum.random(@planetary_classes)
  end

  def random_ship_registry_number() do
    "NCC-#{random_int_between(1000, 9999)}"
  end

  def random_stardate() do
    :rand.uniform() * (42000.0 - 41000.0) + 41000.0
  end

  def format_stardate(stardate) do
    :io_lib.format('~.1f', [stardate])
    |> to_string()
  end

  defp random_int_between(m, n), do: m..n |> Enum.random()
end
