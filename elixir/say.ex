defmodule Say do
  @doc """
  Translate a positive integer into English.
  """
  @units ["", " thousand", " million", " billion", " trillion"]
  @upper_limit 1_000_000_000_000

  @spec in_english(integer) :: {atom, String.t()}
  def in_english(number) when number < 0 or number >= @upper_limit do
    {:error, "number is out of range"}
  end

  def in_english(number) do
    {:ok, convert(number)}
  end

  defp convert(0), do: "zero"
  defp convert(1), do: "one"
  defp convert(2), do: "two"
  defp convert(3), do: "three"
  defp convert(4), do: "four"
  defp convert(5), do: "five"
  defp convert(6), do: "six"
  defp convert(7), do: "seven"
  defp convert(8), do: "eight"
  defp convert(9), do: "nine"
  defp convert(10), do: "ten"
  defp convert(11), do: "eleven"
  defp convert(12), do: "twelve"
  defp convert(13), do: "thirteen"
  defp convert(14), do: "fourteen"
  defp convert(15), do: "fifteen"
  defp convert(16), do: "sixteen"
  defp convert(17), do: "seventeen"
  defp convert(18), do: "eighteen"
  defp convert(19), do: "nineteen"
  defp convert(20), do: "twenty"
  defp convert(30), do: "thirty"
  defp convert(40), do: "forty"
  defp convert(50), do: "fifty"
  defp convert(60), do: "sixty"
  defp convert(70), do: "seventy"
  defp convert(80), do: "eighty"
  defp convert(90), do: "ninety"
  defp convert(number), do: convert_multi_digit(Integer.digits(number))

  defp convert_multi_digit([first, second]) do
    "#{convert(first * 10)}-#{convert(second)}"
  end

  defp convert_multi_digit([0, 0, 0]), do: ""

  defp convert_multi_digit([h, 0, 0]) do
    "#{convert(h)} hundred"
  end

  defp convert_multi_digit([h, t, u]) do
    "#{convert_multi_digit([h, 0, 0])} #{convert(Integer.undigits([t, u]))}"
  end

  defp convert_multi_digit(digits) do
    digits
    |> Enum.reverse()
    |> Stream.chunk_every(3)
    |> Stream.zip(@units)
    |> Stream.map(fn {n, unit} -> convert(n |> Enum.reverse() |> Integer.undigits()) <> unit end)
    |> Stream.drop_while(&String.starts_with?(&1, "zero"))
    |> Enum.reverse()
    |> Enum.join(" ")
  end
end
