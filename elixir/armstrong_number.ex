defmodule ArmstrongNumber do
  @moduledoc """
  Provides a way to validate whether or not a number is an Armstrong number
  """

  @spec valid?(integer) :: boolean
  def valid?(number) do
    digits = number |> Integer.digits()

    exp = length(digits)

    digits
    |> Enum.map(&:math.pow(&1, exp))
    |> Enum.sum()
    |> Kernel.==(number)
  end
end
