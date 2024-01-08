defmodule Knapsack do
  @doc """
  Return the maximum value that a knapsack can carry.
  """
  @spec maximum_value(items :: [%{value: integer, weight: integer}], maximum_weight :: integer) ::
          integer
  def maximum_value([], _maximum_weight) do
    0
  end

  def maximum_value([%{value: value, weight: weight} | rest], maximum_weight) do
    if weight > maximum_weight do
      maximum_value(rest, maximum_weight)
    else
      max(
        value + maximum_value(rest, maximum_weight - weight),
        maximum_value(rest, maximum_weight)
      )
    end
  end
end
