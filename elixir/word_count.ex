defmodule WordCount do
  @doc """
  Count the number of words in the sentence.

  Words are compared case-insensitively.
  """
  @spec count(String.t()) :: map
  def count(sentence) do

    regex = ~r/[^[:alnum:]\-]/u

    sentence
    |> String.split(regex, [trim: true])
    |> List.foldl(%{}, fn x, acc -> Map.update(acc, String.downcase(x), 1, fn v -> v + 1 end) end)

  end
end

# https://www.bignerdranch.com/blog/elixir-and-unicode-part-1-unicode-and-utf-8-explained/
# https://nerdranchighq.wpengine.com/blog/elixir-and-unicode-part-2-working-with-unicode-strings/
