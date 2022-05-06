defmodule Sublist do
  @type comparison :: :equal | :unequal | :sublist | :superlist

  @spec compare([any], [any]) :: comparison
  @doc """
  Returns whether the first list is a sublist or a superlist of the second list
  and if not whether it is equal or unequal to the second list.
  """
  def compare([], []), do: :equal
  def compare([], _), do: :sublist
  def compare(_, []), do: :superlist

  def compare(a, b) do
    len_a = length(a)
    len_b = length(b)

    if len_a == len_b do
      check_equality(a, b)
    else
      if len_a < len_b do
        check_sublist(a, len_a, b, len_b)
      else
        check_superlist(a, len_a, b, len_b)
      end
    end
  end

  def check_equality([], []), do: :equal
  def check_equality([], _), do: :unequal
  def check_equality(_, []), do: :unequal
  def check_equality([x | _xs], [y | _ys]) when x !== y, do: :unequal
  def check_equality([_x | xs], [_y | ys]), do: check_equality(xs, ys)

  def check_sublist(a, len_a, b, _len_b) do
    b
    |> any_window_equals?(a, len_a)
    |> then(fn
      true -> :sublist
      _ -> :unequal
    end)
  end

  def check_superlist(a, _len_a, b, len_b) do
    a
    |> any_window_equals?(b, len_b)
    |> then(fn
      true -> :superlist
      _ -> :unequal
    end)
  end

  @spec any_window_equals?([any], [any], pos_integer) :: boolean
  def any_window_equals?(a, b, len_b) do
    a
    |> Enum.chunk_every(len_b, 1)
    |> Enum.any?(&(check_equality(&1, b) == :equal))
  end
end
