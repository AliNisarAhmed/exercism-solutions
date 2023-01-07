defmodule RotationalCipher do
  @doc """
  Given a plaintext and amount to shift by, return a rotated string.

  Example:
  iex> RotationalCipher.rotate("Attack at dawn", 13)
  "Nggnpx ng qnja"
  """
  @spec rotate(text :: String.t(), shift :: integer) :: String.t()
  def rotate(text, shift) do
    text
    |> String.to_charlist()
    |> Enum.map(fn i -> convert(i, shift) end)
    |> List.to_string()
  end

  defp convert(i, shift) when i >= ?A and i <= ?Z do
    rem(i + shift - ?A, 26) + ?A
  end

  defp convert(i, shift) when i >= ?a and i <= ?z do
    rem(i + shift - ?a, 26) + ?a
  end

  defp convert(i, __shift), do: i
end
