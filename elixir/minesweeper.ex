defmodule Minesweeper do
  @doc """
  Annotate empty spots next to mines with the number of mines next to them.
  """
  @spec annotate([String.t()]) :: [String.t()]
  def annotate([]), do: []

  def annotate(board) do
    row_size = length(board)
    col_size = String.length(Enum.at(board, 0))

    map =
      board
      |> Enum.map(fn s ->
        s
        |> String.split("", trim: true)
        |> Enum.with_index()
      end)
      |> Enum.with_index()
      |> Enum.reduce(%{}, fn {row, row_index}, acc ->
        row
        |> Enum.reduce(acc, fn
          {s, col_index}, map -> Map.put(map, {row_index, col_index}, s)
        end)
      end)

    map =
      Enum.reduce(map, map, fn
        {{r, c}, "*"}, acc ->
          neighbors(r, c)
          |> Enum.reduce(acc, fn {nr, nc}, m ->
            case Map.get(m, {nr, nc}) do
              " " -> Map.put(m, {nr, nc}, 1)
              k when is_integer(k) -> Map.put(m, {nr, nc}, k + 1)
              _ -> m
            end
          end)

        _, acc ->
          acc
      end)

    for r <- 0..(row_size - 1) do
      for c <- 0..(col_size - 1) do
        Map.get(map, {r, c})
      end
    end
    |> Enum.map(&Enum.join/1)
  end

  defp neighbors(r, c) do
    [
      {r - 1, c},
      {r + 1, c},
      {r, c - 1},
      {r, c + 1},
      {r - 1, c - 1},
      {r + 1, c - 1},
      {r - 1, c + 1},
      {r + 1, c + 1}
    ]
  end
end
