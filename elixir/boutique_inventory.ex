defmodule BoutiqueInventory do
  def sort_by_price(inventory) do
    inventory
    |> Enum.sort_by(fn item -> item.price end)
  end

  def with_missing_price(inventory) do
    inventory
    |> Enum.filter(fn item -> item.price == nil end)
  end

  def increase_quantity(%{quantity_by_size: quantity_by_size} = item, count) do
    new_quantities =
      quantity_by_size
      |> Enum.map(fn {k, v} -> {k, v + count} end)
      |> Enum.into(%{})

    Map.put(item, :quantity_by_size, new_quantities)
  end

  def total_quantity(%{quantity_by_size: quantity_by_size}) do
    quantity_by_size
    |> Enum.reduce(0, fn {_, v}, acc -> acc + v end)
  end
end
