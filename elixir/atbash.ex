defmodule Atbash do
  @doc """
  Encode a given plaintext to the corresponding ciphertext

  ## Examples

  iex> Atbash.encode("completely insecure")
  "xlnko vgvob rmhvx fiv"
  """
  @spec encode(String.t()) :: String.t()
  def encode(plaintext) do
    plaintext
    |> String.replace(~r/[^[:alnum:]]/, "")
    |> String.downcase()
    |> String.to_charlist()
    |> Enum.map(&encode_char/1)
    |> Enum.chunk_every(5)
    |> Enum.map(&List.to_string/1)
    |> Enum.join(" ")
  end

  def encode_char(c) when c in ?0..?9, do: c

  def encode_char(c) do
    c + 25 - 2 * (c - ?a)
    # or ?z - c + ?a
  end

  @spec decode(String.t()) :: String.t()
  def decode(cipher) do
    cipher
    |> String.replace(~r/[^[:alnum:]]/, "")
    |> String.to_charlist()
    |> Enum.map(&encode_char/1)
    |> List.to_string()
  end
end
