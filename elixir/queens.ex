defmodule Queens do
  @type t :: %Queens{black: {integer, integer}, white: {integer, integer}}
  defstruct [:white, :black]

  @doc """
  Creates a new set of Queens
  """
  @spec new(Keyword.t()) :: Queens.t()
  def new(opts \\ []) do
    white = Keyword.get(opts, :white)
    black = Keyword.get(opts, :black)

    if out_of_bounds?(white) or out_of_bounds?(black) or white == black do
      raise ArgumentError
    else
      %Queens{
        white: white,
        black: black
      }
    end
  end


  defp out_of_bounds?({x, y}) do
    x < 0 or y < 0 or x > 7 or y > 7
  end
  defp out_of_bounds?(_), do: false

  @doc """
  Gives a string representation of the board with
  white and black queen locations shown
  """
  @spec to_string(Queens.t()) :: String.t()
  def to_string(%Queens{white: white, black: black}) do
    board_list =
      for rows <- 0..7 do
        for cols <- 0..7 do
          cond do
            {rows, cols} == white -> "W"
            {rows, cols} == black -> "B"
            true -> "_"
          end
        end
      end

    board_list
    |> Enum.map(&Enum.join(&1, " "))
    |> Enum.join("\n")
  end

  @doc """
  Checks if the queens can attack each other
  """
  @spec can_attack?(Queens.t()) :: boolean
  def can_attack?(%Queens{black: {x, _}, white: {x, _}}), do: true
  def can_attack?(%Queens{black: {_, y}, white: {_, y}}), do: true

  def can_attack?(%Queens{black: {bx, by}, white: {wx, wy}}) do
    abs(bx - wx) == abs(by - wy)
  end

  def can_attack?(_), do: false
end
