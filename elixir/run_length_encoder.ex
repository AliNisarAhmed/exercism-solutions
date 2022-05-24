defmodule RunLengthEncoder do
  @doc """
  Generates a string where consecutive elements are represented as a data value and count.
  "AABBBCCCC" => "2A3B4C"
  For this example, assume all input are strings, that are all uppercase letters.
  It should also be able to reconstruct the data into its original form.
  "2A3B4C" => "AABBBCCCC"
  """
  @spec encode(String.t()) :: String.t()
  def encode(""), do: ""

  def encode(string) do
    string
    |> String.graphemes()
    |> group_consecutive()
    |> Enum.map(fn
      {c, 1} -> c
      {c, count} -> "#{count}#{c}"
    end)
    |> Enum.join()
  end

  defp group_consecutive(list) do
    list
    |> Enum.chunk_while({List.first(list), 0}, &chunk/2, &{:cont, &1, 0})
  end

  defp chunk(char, {last, count}) do
    if last == char,
      do: {:cont, {char, count + 1}},
      else: {:cont, {last, count}, {char, 1}}
  end

  @spec decode(String.t()) :: String.t()
  def decode(""), do: ""

  def decode(string) do
    ~r/[0-9]+[a-zA-Z]|[a-zA-Z]/
    |> Regex.split(string, include_captures: true, trim: true)
    |> expand()
    |> Enum.reverse()
    |> Enum.join()
  end

  defp expand(list, acc \\ [])
  defp expand([], acc), do: acc

  defp expand([x | xs], acc) do
    case Integer.parse(x) do
      {count, char} -> expand(xs, [String.duplicate(char, count) | acc])
      :error -> expand(xs, [x | acc])
    end
  end
end
