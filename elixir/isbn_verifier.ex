defmodule IsbnVerifier do
  @doc """
    Checks if a string is a valid ISBN-10 identifier

    ## Examples

      iex> IsbnVerifier.isbn?("3-598-21507-X")
      true

      iex> IsbnVerifier.isbn?("3-598-2K507-0")
      false

  """

  # NOTE: NOT MY SOLUTION - my solution is in iteration 1
  @spec isbn?(String.t()) :: boolean
  def isbn?(isbn), do: isbn?(isbn, 10, 0)

  def isbn?(<<c, rest::binary>>, n, sum) when c in ?0..?9,
    do: isbn?(rest, n - 1, sum + n * (c - ?0))

  def isbn?(<<c, rest::binary>>, n, sum) when c == ?-, do: isbn?(rest, n, sum)
  def isbn?("X", _, sum), do: isbn?(<<>>, 0, sum + 10)
  def isbn?(<<>>, 0, sum), do: rem(sum, 11) == 0
  def isbn?(_, _, _), do: false
end
