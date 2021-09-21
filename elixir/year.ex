defmodule Year do
  @doc """
  Returns whether 'year' is a leap year.

  A leap year occurs:

  on every year that is evenly divisible by 4
    except every year that is evenly divisible by 100
      unless the year is also evenly divisible by 400
  """
  @spec leap_year?(non_neg_integer) :: boolean
  def leap_year?(year) do
    div_by_four?(year) and (not div_by_100?(year) or div_by_400?(year)) 
  end

  def div_by_four?(n), do: :math.fmod(n, 4) == 0

  def div_by_100?(n), do: :math.fmod(n, 100) == 0

  def div_by_400?(n), do: :math.fmod(n, 400) == 0
end
