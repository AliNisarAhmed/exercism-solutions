defmodule ListOps do
  # Please don't use any external modules (especially List or Enum) in your
  # implementation. The point of this exercise is to create these basic
  # functions yourself. You may use basic Kernel functions (like `Kernel.+/2`
  # for adding numbers), but please do not use Kernel functions for Lists like
  # `++`, `--`, `hd`, `tl`, `in`, and `length`.

  @spec count(list) :: non_neg_integer
  def count(l) do
    count(l, 0)
  end

  defp count([], n), do: n
  defp count([_x | xs], n), do: count(xs, n + 1)

  @spec reverse(list) :: list
  def reverse(l) do
    reverse(l, [])
  end

  defp reverse([], acc), do: acc
  defp reverse([x | xs], acc), do: reverse(xs, [x | acc])

  @spec map(list, (any -> any)) :: list
  def map([], _f), do: []
  def map([x | xs], f) do
    [f.(x) | map(xs, f)]
  end

  @spec filter(list, (any -> as_boolean(term))) :: list
  def filter([], _f), do: []
  def filter([x | xs], f) do
    if f.(x) do
      [x | filter(xs, f)]
    else
      filter(xs, f)
    end
  end

  @type acc :: any
  @spec foldl(list, acc, (any, acc -> acc)) :: acc
  def foldl([], acc, _f), do: acc
  def foldl([x | xs], acc, f) do
    foldl(xs, f.(x, acc), f)
  end

  @spec foldr(list, acc, (any, acc -> acc)) :: acc
  def foldr([], acc, _f), do: acc
  def foldr([x | xs], acc, f) do
    f.(x, foldr(xs, acc, f))
  end

  @spec append(list, list) :: list
  def append([], b), do: b
  def append(a, []), do: a
  def append([x | xs], b) do
    [x | append(xs, b)]
  end

  @spec concat([[any]]) :: [any]
  def concat([]), do: []
  def concat([x | xs]) do
    append(x, concat(xs))
  end
end
