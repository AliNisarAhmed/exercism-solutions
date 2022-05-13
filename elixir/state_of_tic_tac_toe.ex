defmodule StateOfTicTacToe do
  @doc """
  Determine the state a game of tic-tac-toe where X starts.
  """
  @spec game_state(board :: String.t()) :: {:ok, :win | :ongoing | :draw} | {:error, String.t()}
  def game_state(board_string) do
    board_string
    |> Board.parse_board()
    |> Board.check_state()
  end
end

defmodule Board do
  defstruct rows: [], cols: [], count: %{x: 0, o: 0}

  @spec parse_board(binary) :: %Board{cols: list, count: pos_integer(), rows: list}
  def parse_board(board_string) do
    rows =
      board_string
      |> String.split("\n", trim: true)
      |> Enum.map(fn row ->
        row
        |> String.split("", trim: true)
      end)

    %Board{rows: rows, cols: transpose(rows), count: count_moves(rows)}
  end

  @spec check_state(board :: %Board{}) :: {:ok, :win | :ongoing | :draw} | {:error, String.t()}
  def check_state(board) do
    with :ok <- check_wrong_turn(board) do
      x_win = has_player_won?(board, "X")
      o_win = has_player_won?(board, "O")

      case {x_win, o_win} do
        {true, false} ->
          {:ok, :win}

        {false, true} ->
          {:ok, :win}

        {true, true} ->
          {:error, "Impossible board: game should have ended after the game was won"}

        {false, false} ->
          if is_game_ongoing?(board) do
            {:ok, :ongoing}
          else
            {:ok, :draw}
          end
      end
    end
  end

  defp count_moves(rows) do
    Enum.reduce(rows, %{x: 0, o: 0}, fn row, acc ->
      %{x: row_x, o: row_o} =
        row
        |> Enum.reduce(%{x: 0, o: 0}, fn x, acc ->
          case x do
            "X" -> Map.update(acc, :x, 0, fn n -> n + 1 end)
            "O" -> Map.update(acc, :o, 0, fn n -> n + 1 end)
            _ -> acc
          end
        end)

      acc
      |> Map.update(:x, 0, fn n -> n + row_x end)
      |> Map.update(:o, 0, fn n -> n + row_o end)
    end)
  end

  defp is_game_ongoing?(%Board{count: %{x: 5, o: 4}}), do: false
  defp is_game_ongoing?(%Board{count: %{x: 4, o: 5}}), do: false
  defp is_game_ongoing?(_), do: true

  defp has_player_won?(%Board{rows: rows, cols: cols}, player) do
    is_straight_win?(rows, player) or
      is_straight_win?(cols, player) or
      is_diagonal_win?(rows, player)
  end

  defp is_straight_win?(list_of_lists, player) do
    list_of_lists
    |> Enum.any?(fn list ->
      list
      |> Enum.all?(fn i -> i == player end)
    end)
  end

  defp is_diagonal_win?(rows, player) do
    is_falling_diag_win?(rows, player) or is_rising_diag_win?(rows, player)
  end

  defp is_falling_diag_win?(rows, player) do
    falling_diag =
      for i <- 0..2 do
        rows
        |> Enum.at(i)
        |> Enum.at(i)
      end

    Enum.all?(falling_diag, &(&1 == player))
  end

  defp is_rising_diag_win?(rows, player) do
    rising_diag =
      for {i, j} <-
            2..0
            |> Enum.zip(0..2) do
        rows
        |> Enum.at(i)
        |> Enum.at(j)
      end

    Enum.all?(rising_diag, &(&1 == player))
  end

  @spec check_wrong_turn(board :: %Board{}) :: :ok | {:error, String.t()}
  defp check_wrong_turn(%Board{count: %{x: x, o: o}}) when x - o >= 2,
    do: {:error, "Wrong turn order: X went twice"}

  defp check_wrong_turn(%Board{count: %{x: x, o: o}}) when o > x,
    do: {:error, "Wrong turn order: O started"}

  defp check_wrong_turn(_), do: :ok

  defp transpose(list_of_lists) do
    list_of_lists
    |> Enum.zip()
    |> Enum.map(&Tuple.to_list/1)
  end
end
