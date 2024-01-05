defmodule OcrNumbers do
  @doc """
  Given a 3 x 4 grid of pipes, underscores, and spaces, determine which number is represented, or
  whether it is garbled.
  """

  @spec convert([String.t()]) :: {:ok, String.t()} | {:error, String.t()}
  def convert(input) do
    numbers =
      input
      |> Enum.chunk_every(4, 4, :discard)
      |> Enum.map(&parse_row/1)

    case numbers do
      [] ->
        {:error, "invalid line count"}

      [nil] ->
        {:error, "invalid column count"}

      numbers ->
        {:ok,
         numbers
         |> Enum.map(&Enum.join/1)
         |> Enum.join(",")}
    end
  end

  def parse_row([s1, s2, s3, s4]) do
    parse_numbers(s1, s2, s3, s4, [])
  end

  def parse_numbers("", "", "", "", acc), do: Enum.reverse(acc)

  def parse_numbers(
        <<top::binary-size(3), rest1::binary>>,
        <<middle::binary-size(3), rest2::binary>>,
        <<bottom::binary-size(3), rest3::binary>>,
        <<tail::binary-size(3), rest4::binary>>,
        acc
      ) do
    number = parse_number(top, middle, bottom, tail)

    parse_numbers(rest1, rest2, rest3, rest4, [number | acc])
  end

  def parse_numbers(_, _, _, _, _), do: nil

  def parse_number(" _ ", "| |", "|_|", "   "), do: "0"
  def parse_number("   ", "  |", "  |", "   "), do: "1"
  def parse_number(" _ ", " _|", "|_ ", "   "), do: "2"
  def parse_number(" _ ", " _|", " _|", "   "), do: "3"
  def parse_number("   ", "|_|", "  |", "   "), do: "4"
  def parse_number(" _ ", "|_ ", " _|", "   "), do: "5"
  def parse_number(" _ ", "|_ ", "|_|", "   "), do: "6"
  def parse_number(" _ ", "  |", "  |", "   "), do: "7"
  def parse_number(" _ ", "|_|", "|_|", "   "), do: "8"
  def parse_number(" _ ", "|_|", " _|", "   "), do: "9"
  def parse_number(_, _, _, _), do: "?"
end
