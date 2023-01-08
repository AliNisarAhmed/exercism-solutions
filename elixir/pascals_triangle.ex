defmodule PascalsTriangle do
  @doc """
  Calculates the rows of a pascal triangle
  with the given height
  """
  @spec rows(integer) :: [[integer]]
  def rows(num) do
    0..(num - 1)
    |> Enum.map(&generate_row/1)
  end

  def generate_row(n) do
    0..n
    |> Enum.map(fn r -> choose(n, r) end)
  end

  def choose(_n, 0), do: 1
  def choose(n, 1), do: n
  def choose(n, n), do: 1

  def choose(n, r) do
    n..1
    |> Stream.take(r)
    |> Enum.product()
    |> div(fact(r))
  end

  def fact(r), do: r..1 |> Enum.product()
end
