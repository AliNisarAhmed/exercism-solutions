defmodule Tournament do
  @type match_result :: :win | :loss | :draw | :invalid
  @type match :: %{
          home_team: String.t(),
          away_team: String.t(),
          match_result: match_result()
        }

  @doc """
  Given `input` lines representing two teams and whether the first of them won,
  lost, or reached a draw, separated by semicolons, calculate the statistics
  for each team's number of games played, won, drawn, lost, and total points
  for the season, and return a nicely-formatted string table.

  A win earns a team 3 points, a draw earns 1 point, and a loss earns nothing.

  Order the outcome by most total points for the season, and settle ties by
  listing the teams in alphabetical order.
  """
  @spec tally(input :: list(String.t())) :: String.t()
  def tally([]), do: Tabulate.header()

  def tally(input) do
    input
    |> parse()
    |> Tally.tally_matches()
    |> Tabulate.tabulate_results()
  end

  defp parse(input_list) do
    input_list
    |> Enum.map(&parse_match/1)
  end

  @spec parse_match(String.t()) :: match
  defp parse_match(match_string) do
    with [home_team, away_team, result] <-
           String.split(match_string, ";", trim: true) do
      %{
        home_team: home_team,
        away_team: away_team,
        match_result: parse_match_result(result)
      }
    else
      _ -> nil
    end
  end

  @spec parse_match_result(String.t()) :: match_result()
  defp parse_match_result(string) do
    case string do
      "win" -> :win
      "loss" -> :loss
      "draw" -> :draw
      _ -> :invalid
    end
  end
end

defmodule Tally do
  @moduledoc """
  Computes the tally() type from a list of Tournament.match()
  """
  @type stats :: %{
          team_name: String.t(),
          wins: integer(),
          losses: integer(),
          draws: integer(),
          points: integer()
        }
  @type tally :: %{
          String.t() => stats
        }

  @spec new(String.t()) :: stats()
  def new(team_name) do
    %{
      team_name: team_name,
      wins: 0,
      losses: 0,
      draws: 0,
      points: 0
    }
  end

  @doc """
  Compares two stats objects first with points and then, if points are equal, with team name
  """
  @spec compare_stats(stats, stats) :: boolean()
  def compare_stats(%{points: points_1, team_name: team_name_1}, %{
        points: points_2,
        team_name: team_name_2
      }) do
    points_1 >= points_2 || team_name_1 >= team_name_2
  end

  @spec tally_matches(list(Tournament.match())) :: tally()
  def tally_matches(matches) do
    matches
    |> Enum.reduce(%{}, &tally_match/2)
  end

  def tally_match(nil, tally), do: tally
  def tally_match(%{match_result: :invalid}, tally), do: tally
  def tally_match(
        match,
        tally
      ) do
    tally
    |> update_home_result(match)
    |> update_away_result(match)
  end

  ## --------- PRIVATE -----------

  @spec update_home_result(tally(), Tournament.match()) :: tally()
  defp update_home_result(tally, %{home_team: home_team, match_result: result}) do
    tally
    |> Map.update(
      home_team,
      new(home_team) |> update_stats(result),
      &update_stats(&1, result)
    )
  end

  defp update_away_result(tally, %{away_team: away_team, match_result: result}) do
    tally
    |> Map.update(
      away_team,
      new(away_team) |> update_stats(result, result_type: :away),
      &update_stats(&1, result, result_type: :away)
    )
  end

  @spec update_stats(stats, Tournament.match_result(), list(any)) :: stats()
  defp update_stats(status, result, opts \\ [])

  defp update_stats(stats, result, result_type: :away) do
    case result do
      :win -> %{stats | losses: stats.losses + 1}
      :loss -> %{stats | wins: stats.wins + 1, points: stats.points + 3}
      :draw -> %{stats | draws: stats.draws + 1, points: stats.points + 1}
    end
  end

  defp update_stats(stats, result, []) do
    case result do
      :win -> %{stats | wins: stats.wins + 1, points: stats.points + 3}
      :loss -> %{stats | losses: stats.losses + 1}
      :draw -> %{stats | draws: stats.draws + 1, points: stats.points + 1}
    end
  end
end

defmodule Tabulate do
  @moduledoc """
  This module is tasked with "printing" a Tournament.tally type to a table

  """
  @separator "|"


  @doc """
  Returns the header of the table
  """
  @spec header() :: String.t()
  def header() do
    tm = format_team_name("Team")
    mp = table_entry("MP")
    w = table_entry("W")
    d = table_entry("D")
    l = table_entry("L")
    pts = table_entry("P", column: :last)

    tm <>
      @separator <>
      mp <> @separator <> w <> @separator <> d <> @separator <> l <> @separator <> pts
  end

  @doc """
  Returns the rows of the table formed by using the data inside the tally input map
  """
  @spec tabulate_results(Tally.tally()) :: String.t()
  def tabulate_results(tally) do
    rows =
      tally
      |> Enum.to_list()
      |> Enum.sort_by(fn {_, stats} -> stats end, &Tally.compare_stats/2)
      |> Enum.map(fn {_, stats} -> tabulate_stats(stats) end)

    [header() | rows]
    |> Enum.join("\n")
    |> String.trim()
  end

  ## ---------- PRIVATE --------------

  @spec tabulate_stats(Tally.stats()) :: String.t()
  defp tabulate_stats(%{
         team_name: team_name,
         wins: wins,
         losses: losses,
         draws: draws,
         points: points
       }) do
    tm = format_team_name(team_name)
    mp = table_entry((wins + losses + draws) |> to_string())
    w = table_entry(wins |> to_string())
    d = table_entry(draws |> to_string())
    l = table_entry(losses |> to_string())
    pts = table_entry(points |> to_string(), column: :last)

    tm <>
      @separator <>
      mp <>
      @separator <>
      w <>
      @separator <>
      d <>
      @separator <>
      l <>
      @separator <>
      pts
  end

  defp format_team_name(str) do
    str
    |> String.pad_trailing(31)
  end

  defp table_entry(str, opts \\ [])

  defp table_entry(str, []) when is_binary(str) do
    str
    |> String.pad_leading(3)
    |> String.pad_trailing(4)
  end

  defp table_entry(str, column: :last) do
    str
    |> String.pad_leading(3)
    |> String.pad_trailing(3)
  end
end
