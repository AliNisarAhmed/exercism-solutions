defmodule Proverb do
  @doc """
  Generate a proverb from a list of strings.
  """
  @spec recite(strings :: [String.t()]) :: String.t()
  def recite([]), do: ""
  def recite([x]), do: recite_last_verse(x) <> "\n"

  def recite([first | _rest] = strings) do
    strings
    |> Enum.chunk_every(2, 1, :discard)
    |> Enum.map(&recite_verse/1)
    |> Enum.concat([recite_last_verse(first)])
    |> Enum.join("\n")
    |> then(fn s -> s <> "\n" end)
  end

  def recite_last_verse(x) do
    "And all for the want of a #{x}."
  end

  def recite_verse([first_noun, second_noun]) do
    "For want of a #{first_noun} the #{second_noun} was lost."
  end
end
