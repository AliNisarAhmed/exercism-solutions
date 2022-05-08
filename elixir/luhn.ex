defmodule Luhn do
  @doc """
  Checks if the given number is valid via the luhn formula
  """
  @spec valid?(String.t()) :: boolean
  def valid?(number) do
    with {:ok, str} <- validate_input(number) do
      str
      |> sanitize_input()
      |> Enum.reverse()
      |> Enum.with_index(fn e, i -> {i, String.to_integer(e)} end)
      |> Enum.map(&luhn_double/1)
      |> Enum.sum()
      |> Kernel.rem(10)
      |> Kernel.==(0)
    else
      _ -> false
    end
  end

  defp luhn_double({index, n}) when rem(index, 2) != 0 and n * 2 > 9, do: n * 2 - 9
  defp luhn_double({index, n}) when rem(index, 2) != 0, do: n * 2
  defp luhn_double({_i, n}), do: n

  defp sanitize_input(str) do
    str
    |> String.replace(~r/\s/, "")
    |> String.split("", trim: true)
  end

  defp validate_input(str) do
    str = String.trim(str)

    if String.length(str) > 1 &&
         ~r/^[0-9\s]+$/
         |> Regex.match?(str) do
      {:ok, str}
    else
      {:error, "invalid"}
    end
  end
end
