# This answer was extremely helpful
# https://stackoverflow.com/a/60761975

defmodule Spiral do
  @doc """
  Given the dimension, return a square matrix of numbers in clockwise spiral order.
  """
  @spec matrix(dimension :: integer) :: list(list(integer))
  def matrix(0), do: []

  def matrix(dimension) do
    make_spiral(Enum.to_list(1..(dimension * dimension)), dimension)
  end

  def make_spiral([], _num_rows), do: [[]]
  def make_spiral([x], _num_rows), do: [[x]]

  def make_spiral(xs, num_rows) do
    k = div(length(xs), num_rows)

    [
      Enum.take(xs, k)
      | Enum.drop(xs, k) |> make_spiral(k) |> rotate()
    ]
  end

  # rotates clockwise
  def rotate(rows) do
    rows
    |> Enum.reverse()
    |> transpose()
  end

  def transpose(rows) do
    rows
    |> List.zip()
    |> Enum.map(&Tuple.to_list/1)
  end
end
