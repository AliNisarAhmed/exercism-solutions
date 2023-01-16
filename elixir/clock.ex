defmodule Clock do
  defstruct hour: 0, minute: 0

  defimpl String.Chars, for: Clock do
    def to_string(%Clock{hour: hour, minute: minute}) do
      "#{Clock.format(hour)}:#{Clock.format(minute)}"
    end
  end

  @doc """
  Returns a clock that can be represented as a string:

      iex> Clock.new(8, 9) |> to_string
      "08:09"
  """
  @spec new(integer, integer) :: Clock
  def new(hour, minute) do
    {hours, minutes} = normalize_time(hour, minute)

    %Clock{
      hour: hours,
      minute: minutes
    }
  end

  @doc """
  Adds two clock times:

      iex> Clock.new(10, 0) |> Clock.add(3) |> to_string
      "10:03"
  """
  @spec add(Clock, integer) :: Clock
  def add(%Clock{hour: hour, minute: minute}, add_minute) do
    Clock.new(hour, minute + add_minute)
  end

  # -------------------------------------------------------------------------

  def format(integer) do
    integer
    |> Integer.to_string()
    |> String.pad_leading(2, "0")
  end

  def normalize_time(hour, minute) do
    {hour_inc, minutes} = normalize_minutes(minute)
    hours = normalize_hours(hour, hour_inc)

    {hours, minutes}
  end

  def normalize_minutes(minutes) do
    hours = Integer.floor_div(minutes, 60)
    minutes = Integer.mod(minutes, 60)

    {hours, minutes}
  end

  def normalize_hours(hours, increment \\ 0) do
    Integer.mod(hours + increment, 24)
  end
end
