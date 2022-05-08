defmodule Prime do
  @doc """
  Generates the nth prime.
  """
  @spec nth(non_neg_integer) :: non_neg_integer
  def nth(0), do: raise "there's not 0th prime"
  def nth(count) do
    Stream.unfold(2, &{&1, &1 + 1})
    |> Stream.filter(&is_prime?/1)
    |> Stream.drop(count - 1)
    |> Stream.take(1)
    |> Enum.at(0)
  end

  def is_prime?(1), do: false
  def is_prime?(2), do: true
  def is_prime?(3), do: true
  def is_prime?(n) do
    limit =
      n
      |> :math.sqrt()
      |> trunc()

    2..limit
    |> Enum.map(fn d -> rem(n, d) == 0 end)
    |> Enum.any?()
    |> Kernel.not()
  end
end
