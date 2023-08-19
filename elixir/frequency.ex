defmodule Frequency do
  @doc """
  Count letter frequency in parallel.

  Returns a map of characters to frequencies.

  The number of worker processes to use can be set with 'workers'.
  """
  @spec frequency([String.t()], pos_integer) :: map
  def frequency(texts, workers) do
    texts
    |> Task.async_stream(
      &count_letters/1,
      max_concurrency: workers,
      ordered: false
    )
    |> Enum.reduce(%{}, &merge_counts/2)
  end

  def merge_counts({:ok, count}, acc) do
    Map.merge(acc, count, fn _key, c1, c2 -> c1 + c2 end)
  end

  def count_letters(texts) do
    texts
    |> String.downcase()
    |> String.graphemes()
    |> Enum.filter(&unicode_character?/1)
    |> Enum.frequencies()
  end

  def unicode_character?(char) do
    # https://exercism.org/blog/unicode-matching-in-elixir
    # https://www.toptechskills.com/elixir-phoenix-tutorials-courses/how-to-match-any-unicode-letter-with-regex-elixir/#more-cool-stuff-you-can-match-with-unicode
    String.match?(char, ~r/\p{L}/)
  end
end
