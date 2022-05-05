defmodule SpaceAge do
  @earth_year 31_557_600

  @planet_orbital_period %{
    mercury: 0.2408467,
    venus: 0.61519726,
    earth: 1,
    mars: 1.8808158,
    jupiter: 11.862615,
    saturn: 29.447498,
    uranus: 84.016846,
    neptune: 164.79132
  }

  @type planet ::
          :mercury
          | :venus
          | :earth
          | :mars
          | :jupiter
          | :saturn
          | :uranus
          | :neptune

  @doc """
  Return the number of years a person that has lived for 'seconds' seconds is
  aged on 'planet', or an error if 'planet' is not a planet.
  """
  @spec age_on(planet, pos_integer) :: {:ok, float} | {:error, String.t()}
  def age_on(planet, seconds) when is_map_key(@planet_orbital_period, planet) do
    {:ok, (seconds / (@earth_year * @planet_orbital_period[planet])) |> format_years()}
  end

  def age_on(_planet, _seconds), do: {:error, "not a planet"}

  def format_years({:ok, year}), do: {:ok, year |> Float.round(2)}
  def format_years(v), do: v
end
