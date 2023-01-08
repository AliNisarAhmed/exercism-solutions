defmodule MatchingBrackets do
  @doc """
  Checks that all the brackets and braces in the string are matched correctly, and nested correctly
  """
  @opening ["[", "{", "("]
  @closing ["]", "}", ")"]

  @spec check_brackets(String.t()) :: boolean
  def check_brackets(str) do
    str
    |> String.graphemes()
    |> Enum.reduce_while([], &reducer/2)
    |> then(fn
      [] -> true
      _ -> false
    end)
  end

  defp reducer(bracket, stack) when bracket in @opening do
    {:cont, [bracket | stack]}
  end

  defp reducer(bracket, []) when bracket in @closing do
    {:halt, [bracket]}
  end

  defp reducer("}", ["{" | rest]), do: {:cont, rest}
  defp reducer("}", [_last | _rest] = stack), do: {:halt, stack}
  defp reducer(")", ["(" | rest]), do: {:cont, rest}
  defp reducer(")", [_last | _rest] = stack), do: {:halt, stack}
  defp reducer("]", ["[" | rest]), do: {:cont, rest}
  defp reducer("]", [_last | _rest] = stack), do: {:halt, stack}

  defp reducer(_, stack), do: {:cont, stack}
end
