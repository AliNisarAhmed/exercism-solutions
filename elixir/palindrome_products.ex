defmodule PalindromeProducts do
  @doc """
  Generates all palindrome products from an optionally given min factor (or 1) to a given max factor.

  return type is a map of shape 
  %{
    <smallest_palindrome>: List<[factor1, factor2]>,
    <largest_palindrome>: List<[factor1, factor2]>
  }
  """

  @init_acc %{
    smallest: {:infinity},
    smallest_factors: [],
    largest: -1,
    largest_factors: []
  }

  @spec generate(non_neg_integer, non_neg_integer) :: map
  def generate(max_factor, min_factor \\ 1)

  def generate(max_factor, min_factor) when max_factor < min_factor do
    raise ArgumentError
  end

  def generate(max_factor, min_factor) do
    generate(min_factor, min_factor, min_factor, max_factor, @init_acc)
  end

  def generate(f1, _f2, _min_factor, max_factor, acc) when f1 > max_factor do
    if acc.largest == -1 do
      %{}
    else
      %{
        acc.smallest => Enum.sort(acc.smallest_factors),
        acc.largest => Enum.sort(acc.largest_factors)
      }
    end
  end

  def generate(f1, f2, min_factor, max_factor, acc) when f2 > max_factor do
    generate(f1 + 1, f1 + 1, min_factor, max_factor, acc)
  end

  def generate(
        f1,
        f2,
        min_factor,
        max_factor,
        %{smallest: smallest} = acc
      )
      when f1 * f2 == smallest do
    new_acc =
      acc
      |> Map.update!(:smallest_factors, fn factors -> [[f1, f2] | factors] end)

    generate(f1, f2 + 1, min_factor, max_factor, new_acc)
  end

  def generate(
        f1,
        f2,
        min_factor,
        max_factor,
        %{smallest: smallest} = acc
      )
      when f1 * f2 < smallest do
    product = f1 * f2

    if is_palindrome?(product) do
      new_acc =
        acc
        |> Map.replace(:smallest, product)
        |> Map.replace(:smallest_factors, [[f1, f2]])

      generate(f1, f2 + 1, min_factor, max_factor, new_acc)
    else
      generate(f1, f2 + 1, min_factor, max_factor, acc)
    end
  end

  def generate(
        f1,
        f2,
        min_factor,
        max_factor,
        %{largest: largest} = acc
      )
      when f1 * f2 == largest do
    new_acc =
      acc
      |> Map.update!(:largest_factors, fn factors -> [[f1, f2] | factors] end)

    generate(f1, f2 + 1, min_factor, max_factor, new_acc)
  end

  def generate(
        f1,
        f2,
        min_factor,
        max_factor,
        %{largest: largest} = acc
      )
      when f1 * f2 > largest do
    product = f1 * f2

    if is_palindrome?(product) do
      new_acc =
        acc
        |> Map.replace(:largest, product)
        |> Map.replace(:largest_factors, [[f1, f2]])

      generate(f1, f2 + 1, min_factor, max_factor, new_acc)
    else
      generate(f1, f2 + 1, min_factor, max_factor, acc)
    end
  end

  def generate(f1, f2, min_factor, max_factor, acc) do
    generate(f1, f2 + 1, min_factor, max_factor, acc)
  end

  def is_palindrome?(n) do
    is_palindrome?(n, 0, n)
  end

  def is_palindrome?(n, rev, original_num) when n == 0 do
    rev == original_num
  end

  def is_palindrome?(n, rev, original_num) do
    digit = rem(n, 10)
    rev = rev * 10 + digit
    is_palindrome?(div(n, 10), rev, original_num)
  end
end
