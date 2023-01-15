defmodule CryptoSquare do
  @doc """
  Encode string square methods
  ## Examples

    iex> CryptoSquare.encode("abcd")
    "ac bd"
  """

  @spec encode(String.t()) :: String.t()
  def encode(""), do: ""

  def encode(str) do
    str =
      str
      |> sanitize()

    chunk_size = calc_chunk_size(str)

    str
    |> String.graphemes()
    |> Enum.chunk_every(chunk_size, chunk_size, Stream.cycle([" "]))
    |> transpose()
    |> Enum.map(&Enum.join/1)
    |> Enum.join(" ")
  end

  def sanitize(str) do
    str
    |> String.downcase()
    |> String.replace(~r/[^[:alnum:]]/, "")
  end

  def transpose(rows) do
    rows
    |> List.zip()
    |> Enum.map(&Tuple.to_list/1)
  end

  def calc_chunk_size(str) do
    str
    |> String.length()
    |> :math.sqrt()
    |> ceil
  end
end
