defmodule Series do
  @doc """
  Finds the largest product of a given number of consecutive numbers in a given string of numbers.
  """
  @spec largest_product(String.t(), non_neg_integer) :: non_neg_integer
  def largest_product(_string, 0), do: 1
  def largest_product(_string, n) when n < 0, do: raise(ArgumentError)

  def largest_product(number_string, size) do
    if size > String.length(number_string) or not Regex.match?(~r/^[\d]+$/, number_string) do
      raise ArgumentError
    else
      number_string
      |> String.split("", trim: true)
      |> Enum.map(&String.to_integer/1)
      |> Enum.chunk_every(size, 1, :discard)
      |> Enum.map(&Enum.product/1)
      |> Enum.max()
    end
  end
end
