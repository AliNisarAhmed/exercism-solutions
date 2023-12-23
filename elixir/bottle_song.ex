defmodule BottleSong do
  @moduledoc """
  Handles lyrics of the popular children song: Ten Green Bottles
  """

  @numbers_english %{
    0 => "no",
    1 => "one",
    2 => "two",
    3 => "three",
    4 => "four",
    5 => "five",
    6 => "six",
    7 => "seven",
    8 => "eight",
    9 => "nine",
    10 => "ten"
  }

  @spec recite(pos_integer, pos_integer) :: String.t()
  def recite(_start_bottle, 0) do
    ""
  end

  def recite(start_bottle, take_down) do
    start_bottle..(start_bottle - take_down + 1)
    |> Enum.map(fn n ->
      verse(n)
    end)
    |> Enum.join("\n\n")
  end

  defp verse(start_bottle) do
    start_number = Map.get(@numbers_english, start_bottle) |> String.capitalize()
    end_number = Map.get(@numbers_english, start_bottle - 1)

    verse_1 = "#{start_number} green #{bottles(start_bottle)} hanging on the wall,"
    verse_3 = "And if one green bottle should accidentally fall,"
    verse_4 = "There'll be #{end_number} green #{bottles(start_bottle - 1)} hanging on the wall."

    Enum.join([verse_1, verse_1, verse_3, verse_4], "\n")
  end

  defp bottles(1), do: "bottle"
  defp bottles(_n), do: "bottles"
end
