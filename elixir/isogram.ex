defmodule Isogram do
  @doc """
  Determines if a word or sentence is an isogram
  """
  @spec isogram?(String.t()) :: boolean
  def isogram?(sentence) do
    sentence
    |> String.downcase()
    |> String.to_charlist()
    |> Enum.filter(fn c -> c in ?a..?z end)
    |> Enum.reduce_while(%{}, fn c, acc ->
      case Map.get(acc, c) do
        nil -> {:cont, Map.put(acc, c, true)}
        _ -> {:halt, false}
      end
    end)
  end
end
