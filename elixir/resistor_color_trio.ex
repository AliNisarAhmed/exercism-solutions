defmodule ResistorColorTrio do
  @doc """
  Calculate the resistance value in ohm or kiloohm from resistor colors
  """

  @color_codes %{
    black: 0,
    brown: 1,
    red: 2,
    orange: 3,
    yellow: 4,
    green: 5,
    blue: 6,
    violet: 7,
    grey: 8,
    white: 9
  }

  @spec label(colors :: [atom]) :: {number, :ohms | :kiloohms}
  def label(colors) when is_list(colors) do
    colors
    |> Enum.take(3)
    |> Enum.map(&@color_codes[&1])
    |> then(fn [color1, color2, color3] ->
      [color1, color2]
      |> Integer.undigits()
      |> Kernel.*(:math.pow(10, color3))
    end)
    |> add_units()
  end

  @spec add_units(n :: number) :: {number, :ohms | :kiloohms}
  def add_units(n) when n >= 1000 do
    {n / 1000, :kiloohms}
  end

  def add_units(n), do: {n, :ohms}
end
