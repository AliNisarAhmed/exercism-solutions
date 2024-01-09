defmodule Dominoes do
  @type domino :: {1..6, 1..6}

  @doc """
  chain?/1 takes a list of domino stones and returns boolean indicating if it's
  possible to make a full chain
  """
  @spec chain?(dominoes :: [domino]) :: boolean
  def chain?([]), do: true

  def chain?(dominoes) do
    adj_list = to_adj_list(dominoes)

    not any_odd_degree?(adj_list) and connected?(adj_list)
  end

  defp to_adj_list(dominoes) do
    dominoes
    |> Enum.reduce(%{}, fn {x, y}, map ->
      map
      |> Map.update(x, [y], fn list -> [y | list] end)
      |> Map.update(y, [x], fn list -> [x | list] end)
    end)
  end

  defp any_odd_degree?(adj_list) do
    adj_list
    |> Map.values()
    |> Enum.any?(fn list -> rem(length(list), 2) != 0 end)
  end

  defp connected?(adj_list) do
    visited =
      adj_list
      |> Enum.map(fn {k, _v} -> {k, false} end)
      |> Enum.into(%{})

    first =
      visited
      |> Map.keys()
      |> List.first()

    connected?(adj_list, visited, [first])
  end

  defp connected?(_adj_list, visited, []) do
    Enum.all?(visited, fn {_k, v} -> v == true end)
  end

  defp connected?(adj_list, visited, [top | rest]) do
    visited =
      visited
      |> Map.put(top, true)

    neighbs =
      adj_list
      |> Map.get(top, [])
      |> Enum.filter(fn x -> not Map.get(visited, x, false) end)

    connected?(adj_list, visited, neighbs ++ rest)
  end
end
