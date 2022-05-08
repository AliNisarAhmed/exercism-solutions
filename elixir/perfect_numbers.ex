defmodule PerfectNumbers do
  @type number_kind :: :perfect | :abundant | :deficient
  @doc """
  Determine the aliquot sum of the given `number`, by summing all the factors
  of `number`, aside from `number` itself.

  Based on this sum, classify the number as:

  :perfect if the aliquot sum is equal to `number`
  :abundant if the aliquot sum is greater than `number`
  :deficient if the aliquot sum is less than `number`
  """
  @spec classify(number :: integer) :: {:ok, number_kind} | {:error, String.t()}
  def classify(n) when n <= 0 do
    {:error, "Classification is only possible for natural numbers."}
  end

  def classify(1), do: {:ok, :deficient}

  def classify(number) do
    number
    |> aliquot_sum()
    |> check_kind(number)
  end

  def aliquot_sum(n) do
    1..div(n, 2)
    |> Enum.filter(&(rem(n, &1) == 0))
    |> Enum.sum()
  end

  def check_kind(s, n) when s > n, do: {:ok, :abundant}
  def check_kind(s, n) when s == n, do: {:ok, :perfect}
  def check_kind(_s, _n), do: {:ok, :deficient}
end
