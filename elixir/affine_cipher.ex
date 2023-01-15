defmodule AffineCipher do
  @typedoc """
  A type for the encryption key
  """
  @type key() :: %{a: integer, b: integer}

  @first_letter ?a
  @last_letter ?z
  @m 26

  @doc """
  Encode an encrypted message using a key
  """
  @spec encode(key :: key(), message :: String.t()) :: {:ok, String.t()} | {:error, String.t()}
  def encode(%{a: a, b: b}, message) do
    if not coprime?(a) do
      {:error, "a and m must be coprime."}
    else
      encoded =
        message
        |> String.replace(~r/[^[:alnum:]]/, "")
        |> String.downcase()
        |> String.to_charlist()
        |> Enum.map(fn
          c when c >= @first_letter and c <= @last_letter ->
            Integer.mod(a * (c - @first_letter) + b, @m) + @first_letter

          c ->
            c
        end)
        |> Enum.chunk_every(5)
        |> Enum.map(&List.to_string/1)
        |> Enum.join(" ")

      {:ok, encoded}
    end
  end

  def coprime?(a) do
    Integer.gcd(a, @m) == 1
  end

  @doc """
  Decode an encrypted message using a key
  """
  @spec decode(key :: key(), message :: String.t()) :: {:ok, String.t()} | {:error, String.t()}
  def decode(%{a: a, b: b}, encrypted) do
    if not coprime?(a) do
      {:error, "a and m must be coprime."}
    else
      {_, x, _} = Integer.extended_gcd(a, @m)

      decoded =
        encrypted
        |> String.replace(~r/[^[:alnum:]]/, "")
        |> String.to_charlist()
        |> Enum.map(fn
          c when c >= @first_letter and c <= @last_letter ->
            Integer.mod(x * (c - @first_letter - b), 26) + @first_letter

          c ->
            c
        end)
        |> List.to_string()

      {:ok, decoded}
    end
  end
end
