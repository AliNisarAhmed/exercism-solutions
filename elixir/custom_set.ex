defmodule CustomSet do
  @opaque t :: %__MODULE__{map: map}

  defstruct map: %{}

  @spec new(Enum.t()) :: t
  def new(enumerable) do
    %__MODULE__{map: Enum.into(enumerable, %{}, fn k -> {k, true} end)}
  end

  @spec empty?(t) :: boolean
  def empty?(custom_set) do
    map_size(custom_set.map) == 0
  end

  @spec contains?(t, any) :: boolean
  def contains?(custom_set, element) do
    Map.has_key?(custom_set.map, element)
  end

  @spec subset?(t, t) :: boolean
  def subset?(custom_set_1, custom_set_2) do
    Map.keys(custom_set_1.map)
    |> Enum.all?(fn k -> Map.has_key?(custom_set_2.map, k) end)
  end

  @spec disjoint?(t, t) :: boolean
  def disjoint?(custom_set_1, custom_set_2) do
    Map.keys(custom_set_1.map)
    |> Enum.any?(fn k -> Map.has_key?(custom_set_2.map, k) end)
    |> Kernel.not()
  end

  @spec equal?(t, t) :: boolean
  def equal?(custom_set_1, custom_set_2) do
    subset?(custom_set_1, custom_set_2) and subset?(custom_set_2, custom_set_1)
  end

  @spec add(t, any) :: t
  def add(custom_set, element) do
    %__MODULE__{
      map: Map.put_new(custom_set.map, element, true)
    }
  end

  @spec intersection(t, t) :: t
  def intersection(custom_set_1, custom_set_2) do
    elements_1 = member_other_set(custom_set_1, custom_set_2)
    elements_2 = member_other_set(custom_set_2, custom_set_1)

    new(Enum.concat(elements_1, elements_2))
  end

  @spec member_other_set(t, t) :: Enum.t()
  defp member_other_set(custom_set_1, custom_set_2) do
    Map.keys(custom_set_1.map)
    |> Enum.filter(fn k -> Map.has_key?(custom_set_2.map, k) end)
  end

  @spec difference(t, t) :: t
  def difference(custom_set_1, custom_set_2) do
    new(
      Map.keys(custom_set_1.map)
      |> Enum.reject(fn k -> Map.has_key?(custom_set_2.map, k) end)
    )
  end

  @spec union(t, t) :: t
  def union(custom_set_1, custom_set_2) do
    %__MODULE__{
      map:
        Enum.concat(
          custom_set_1.map,
          custom_set_2.map
        )
        |> Enum.into(%{})
    }
  end
end
