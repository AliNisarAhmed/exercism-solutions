defmodule Meetup do
  @weekdays %{
    monday: 1,
    tuesday: 2,
    wednesday: 3,
    thursday: 4,
    friday: 5,
    saturday: 6,
    sunday: 7
  }
  @moduledoc """
  Calculate meetup dates.
  """

  @type weekday ::
          :monday
          | :tuesday
          | :wednesday
          | :thursday
          | :friday
          | :saturday
          | :sunday

  @type schedule :: :first | :second | :third | :fourth | :last | :teenth

  @doc """
  Calculate a meetup date.

  The schedule is in which week (1..4, last or "teenth") the meetup date should
  fall.
  """
  @spec meetup(pos_integer, pos_integer, weekday, schedule) :: :calendar.date()
  def meetup(year, month, weekday, :teenth) do
    find_date_in_range(13..19, year, month, weekday)
  end

  def meetup(year, month, weekday, :first) do
    find_date_in_range(1..7, year, month, weekday)
  end

  def meetup(year, month, weekday, :second) do
    find_date_in_range(8..14, year, month, weekday)
  end

  def meetup(year, month, weekday, :third) do
    find_date_in_range(15..21, year, month, weekday)
  end

  def meetup(year, month, weekday, :fourth) do
    find_date_in_range(22..28, year, month, weekday)
  end

  def meetup(year, month, weekday, :last) do
    days_in_month =
      Date.new(year, month, 1)
      |> then(fn {:ok, date} -> Date.days_in_month(date) end)

    find_date_in_range(days_in_month..(days_in_month - 7), year, month, weekday)
  end

  defp find_date_in_range(range, year, month, weekday) do
    range
    |> Enum.map(fn day -> Date.new(year, month, day) end)
    |> Enum.find(nil, fn {:ok, date} -> Date.day_of_week(date) == @weekdays[weekday] end)
    |> then(fn {:ok, date} -> date end)
  end
end
