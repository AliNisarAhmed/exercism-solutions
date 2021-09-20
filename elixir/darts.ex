defmodule Darts do
  @type position :: {number, number}

  @doc """
  Calculate the score of a single dart hitting a target
  """
  @spec score(position :: position) :: integer
  def score({x, y}) do
    d = distance({x, y})
    cond do 
      d > 10 -> 0
      d <= 10 and d > 5 -> 1
      d <= 5 and d > 1 -> 5 
      true -> 10
    end
  end

  def distance({x, y}) do 
    :math.sqrt(x * x + y * y)
  end
end
