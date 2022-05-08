defmodule Yacht do
  @type category ::
          :ones
          | :twos
          | :threes
          | :fours
          | :fives
          | :sixes
          | :full_house
          | :four_of_a_kind
          | :little_straight
          | :big_straight
          | :choice
          | :yacht

  @score_multipliers %{
    ones: 1,
    twos: 2,
    threes: 3,
    fours: 4,
    fives: 5,
    sixes: 6
  }

  @doc """
  Calculate the score of 5 dice using the given category's scoring method.
  """
  @spec score(category :: category(), dice :: [integer]) :: integer
  def score(category, dice) when category in [:ones, :twos, :threes, :fours, :fives, :sixes] do
    dice
    |> Enum.count(&(&1 == @score_multipliers[category]))
    |> Kernel.*(@score_multipliers[category])
  end

  def score(:yacht, [x, x, x, x, x]), do: 50
  def score(:yacht, _), do: 0

  def score(:full_house, dice) do
    dice
    |> Enum.sort()
    |> score_fullhouse()
  end

  def score(:four_of_a_kind, dice) do
    dice
    |> Enum.sort()
    |> score_four_of_a_kind()
  end

  def score(:little_straight, dice) do
    if Enum.sort(dice) == [1, 2, 3, 4, 5] do
      30
    else
      0
    end
  end

  def score(:big_straight, dice) do
    if Enum.sort(dice) == [2, 3, 4, 5, 6] do
      30
    else
      0
    end
  end

  def score(:choice, dice), do: Enum.sum(dice)

  defp score_fullhouse([x, x, x, y, y]) when x != y, do: 3 * x + 2 * y
  defp score_fullhouse([x, x, y, y, y]) when x != y, do: 2 * x + 3 * y
  defp score_fullhouse(_), do: 0

  defp score_four_of_a_kind([x, x, x, x, _]), do: 4 * x
  defp score_four_of_a_kind([_, x, x, x, x]), do: 4 * x
  defp score_four_of_a_kind(_), do: 0
end
