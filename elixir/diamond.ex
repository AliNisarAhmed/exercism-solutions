defmodule Diamond do
  @doc """
  Given a letter, it prints a diamond starting with 'A',
  with the supplied letter at the widest point.
  """

  @separator ' '

  @spec build_shape(char) :: String.t()
  def build_shape(letter) do
    letter_number =
      letter
      |> calc_letter_number()

    1..letter_number
    |> generate_rows(letter_number)
    |> reflect(letter_number)
    |> join()
  end

  @spec join([String.t()]) :: String.t()
  def join(list_of_strings) do
    list_of_strings
    |> Enum.join("\n")
    |> then(&(&1 <> "\n"))
  end

  @spec reflect([String.t()], integer()) :: [String.t()]
  def reflect(rows, letter_number) do
    Enum.concat(
      rows,
      rows
      |> Enum.take(letter_number - 1)
      |> Enum.reverse()
    )
  end

  @spec generate_rows(Range.t(), pos_integer()) :: [String.t()]
  def generate_rows(range, letter_number) do
    range
    |> Enum.map(fn k -> generate_row(k, letter_number) end)
  end

  @spec generate_row(pos_integer(), pos_integer()) :: String.t()
  def generate_row(row_number, letter_number) do
    num_spaces = letter_number - row_number
    spaces = generate_spaces(num_spaces)

    middle_size = 2 * letter_number - 1 - 2 * (letter_number - row_number)
    middle = generate_middle(middle_size, row_number + 64)

    (spaces ++ middle ++ spaces)
    |> to_string()
  end

  @spec generate_middle(pos_integer(), pos_integer()) :: charlist()
  def generate_middle(1, _letter), do: 'A'

  def generate_middle(size, letter) do
    [letter | generate_spaces(size - 2)] ++ [letter]
  end

  @spec generate_spaces(pos_integer()) :: charlist()
  def generate_spaces(0), do: []

  def generate_spaces(k) do
    1..k
    |> Enum.map(fn _ -> @separator end)
  end

  @spec calc_letter_number(pos_integer()) :: pos_integer()
  def calc_letter_number(letter) do
    letter - ?A + 1
  end
end
