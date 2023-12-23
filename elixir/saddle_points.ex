defmodule SaddlePoints do
  @doc """
  Parses a string representation of a matrix
  to a list of rows
  """
  @spec rows(String.t()) :: [[integer]]
  def rows(str) do
    str
    |> String.split("\n", trim: true)
    |> Enum.map(fn row_str ->
      row_str
      |> String.split(" ", trim: true)
      |> Enum.map(&String.to_integer(&1))
    end)
  end

  @doc """
  Parses a string representation of a matrix
  to a list of columns
  """
  @spec columns(String.t()) :: [[integer]]
  def columns(str) do
    str
    |> rows()
    |> transpose()
  end

  @doc """
  Calculates all the saddle points from a string
  representation of a matrix
  """
  @spec saddle_points(String.t()) :: [{integer, integer}]
  def saddle_points(str) do
    rows = rows(str)
    cols = columns(str)

    row_max = Enum.map(rows, &Enum.max/1)
    col_min = Enum.map(cols, &Enum.min/1)

    for {row, row_index} <- Enum.with_index(rows),
        {col, col_index} <- Enum.with_index(row),
        Enum.at(row_max, row_index) == col,
        Enum.at(col_min, col_index) == col do
      {row_index + 1, col_index + 1}
    end
  end

  # ---------------------

  defp transpose(rows) do
    rows
    |> List.zip()
    |> Enum.map(&Tuple.to_list/1)
  end
end
