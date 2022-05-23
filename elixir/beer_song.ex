defmodule BeerSong do
  @doc """
  Get a single verse of the beer song
  """
  @spec verse(integer) :: String.t()
  def verse(number) do
    [first_clause(number), second_clause(number)]
    |> Enum.join("\n")
  end

  defp first_clause(num_bottles) do
    "#{bottles(num_bottles)} of beer on the wall, #{bottles(num_bottles) |> String.downcase()} of beer."
  end

  defp bottles(0), do: "No more bottles"
  defp bottles(1), do: "1 bottle"
  defp bottles(n), do: "#{n} bottles"

  defp second_clause(0) do
    "Go to the store and buy some more, #{remaining_bottles(0)} of beer on the wall.\n"
  end
  defp second_clause(n) do
    "Take #{take_number(n)} down and pass it around, #{remaining_bottles(n)} of beer on the wall.\n"
  end

  defp take_number(1), do: "it"
  defp take_number(_), do: "one"

  defp remaining_bottles(0), do: "99 bottles"
  defp remaining_bottles(1), do: "no more bottles"
  defp remaining_bottles(2), do: "1 bottle"
  defp remaining_bottles(n), do: "#{n - 1} bottles"

  @doc """
  Get the entire beer song for a given range of numbers of bottles.
  """
  @spec lyrics(Range.t()) :: String.t()
  def lyrics(range \\ 99..0) do
    range
    |> Enum.map(&verse/1)
    |> Enum.join("\n")
  end
end
