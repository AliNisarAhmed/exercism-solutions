defmodule RailFenceCipher do
  @doc """
  Encode a given plaintext to the corresponding rail fence ciphertext
  """
  @spec encode(String.t(), pos_integer) :: String.t()
  def encode("", _rails) do
    ""
  end

  def encode(plaintext, 1), do: plaintext

  def encode(plaintext, k) do
    letters = String.graphemes(plaintext)

    # generate an empty map like %{0 => [], 1 => [], 2 => []}
    map =
      0..(k - 1)
      |> Enum.into(%{}, fn v -> {v, []} end)

    {map, _, _} =
      letters
      |> Enum.reduce({map, 0, :up}, fn
        letter, {m, g, :up} when g == k - 1 ->
          {insert_letter(m, g, letter), g - 1, :down}

        letter, {m, 0, :down} ->
          {insert_letter(m, 0, letter), 1, :up}

        letter, {m, g, :up} ->
          {insert_letter(m, g, letter), g + 1, :up}

        letter, {m, g, :down} ->
          {insert_letter(m, g, letter), g - 1, :down}
      end)

    0..(k - 1)
    |> Enum.map(fn k -> Map.get(map, k) |> Enum.reverse() |> Enum.join() end)
    |> Enum.join()
  end

  defp insert_letter(map, g, letter) do
    Map.update!(map, g, fn list -> [letter | list] end)
  end

  @doc """
  Decode a given rail fence ciphertext to the corresponding plaintext
  """
  @spec decode(String.t(), pos_integer) :: String.t()
  def decode(str, 1), do: str

  def decode(ciphertext, rails) do
    letters = String.graphemes(ciphertext)

    # generate a map which descibes the count of element in each row
    # e.g. %{ 0 => 6, 2 => 11, 3 => 6}
    {map, _, _} =
      Enum.reduce(letters, {%{}, 0, :up}, fn
        _letter, {map, g, :up} when g == rails - 1 ->
          {increment_count(map, g), g - 1, :down}

        _letter, {map, 0, :down} ->
          {increment_count(map, 0), 1, :up}

        _letter, {map, g, :up} ->
          {increment_count(map, g), g + 1, :up}

        _letter, {map, g, :down} ->
          {increment_count(map, g), g - 1, :down}
      end)

    {map, _} =
      map
      |> Enum.reduce({%{}, ciphertext}, fn {k, v}, {m, s} ->
        {current, rest} = String.split_at(s, v)

        {Map.put(m, k, String.graphemes(current)), rest}
      end)

    {result, _, _, _} =
      letters
      |> Enum.reduce({[], 0, :up, map}, fn
        _x, {result, 0, :down, m} ->
          {letter, updated_map} = get_first_letter(m, 0)
          {[letter | result], 1, :up, updated_map}

        _x, {result, g, :up, m} when g == rails - 1 ->
          {letter, updated_map} = get_first_letter(m, g)
          {[letter | result], g - 1, :down, updated_map}

        _x, {result, g, :up, m} ->
          {letter, updated_map} = get_first_letter(m, g)
          {[letter | result], g + 1, :up, updated_map}

        _x, {result, g, :down, m} ->
          {letter, updated_map} = get_first_letter(m, g)
          {[letter | result], g - 1, :down, updated_map}
      end)

    result
    |> Enum.reverse()
    |> Enum.join()
  end

  defp increment_count(map, g) do
    Map.update(map, g, 1, fn v -> v + 1 end)
  end

  defp get_first_letter(map, g) do
    Map.get_and_update(map, g, fn [l | rest] -> {l, rest} end)
  end
end
