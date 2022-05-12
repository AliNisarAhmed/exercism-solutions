defmodule BinarySearch do
  @doc """
    Searches for a key in the tuple using the binary search algorithm.
    It returns :not_found if the key is not in the tuple.
    Otherwise returns {:ok, index}.

    ## Examples

      iex> BinarySearch.search({}, 2)
      :not_found

      iex> BinarySearch.search({1, 3, 5}, 2)
      :not_found

      iex> BinarySearch.search({1, 3, 5}, 5)
      {:ok, 2}

  """

  @spec search(tuple, integer) :: {:ok, integer} | :not_found
  def search({}, _), do: :not_found

  def search(numbers, key) do
    search(numbers, key, 0, tuple_size(numbers) - 1)
  end

  def search(_numbers, _key, left, right) when left > right, do: :not_found

  def search(numbers, key, left, right) do
    mid = midpoint(left, right)
    current = elem(numbers, mid)

    if current == key do
      {:ok, mid}
    else
      if current > key do
        search(numbers, key, left, mid - 1)
      else
        search(numbers, key, mid + 1, right)
      end
    end
  end

  def midpoint(left, right) do
    div(left + right, 2)
  end
end
