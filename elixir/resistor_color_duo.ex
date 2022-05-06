defmodule ResistorColorDuo do
  @doc """
  Calculate a resistance value from two colors
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

  @spec value(colors :: [atom]) :: integer
  def value([color1, color2 | _]) do
    [@color_codes[color1], @color_codes[color2]]
    |> Integer.undigits()
  end
end
