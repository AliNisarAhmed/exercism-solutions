defmodule TwelveDays do
  @doc """
  Given a `number`, return the song's verse for that specific day, including
  all gifts for previous days in the same line.
  """

  @components [
    {"first", "a Partridge in a Pear Tree."},
    {"second", "two Turtle Doves"},
    {"third", "three French Hens"},
    {"fourth", "four Calling Birds"},
    {"fifth", "five Gold Rings"},
    {"sixth", "six Geese-a-Laying"},
    {"seventh", "seven Swans-a-Swimming"},
    {"eighth", "eight Maids-a-Milking"},
    {"ninth", "nine Ladies Dancing"},
    {"tenth", "ten Lords-a-Leaping"},
    {"eleventh", "eleven Pipers Piping"},
    {"twelfth", "twelve Drummers Drumming"}
  ]

  @spec verse(number :: integer) :: String.t()
  def verse(number) do
    @components
    |> Enum.take(number)
    |> Enum.reverse()
    |> form_verse([])
  end

  def form_verse([head | _rest] = parts, acc) do
    first = first_clause(head)

    second =
      second_clause(parts, acc)
      |> Enum.reverse()
      |> Enum.join(", ")

    first <> second
  end

  def first_clause({day, _}) do
    "On the #{day} day of Christmas my true love gave to me: "
  end

  def second_clause([{_day, phrase}], acc) do
    [phrase | acc]
  end

  def second_clause([{_, second_last}, {_, last} | []], acc) do
    acc = [second_last | acc]
    acc = ["and " <> last | acc]
    acc
  end

  def second_clause([{_, phrase} | rest], acc) do
    second_clause(rest, [phrase | acc])
  end

  @doc """
  Given a `starting_verse` and an `ending_verse`, return the verses for each
  included day, one per line.
  """
  @spec verses(starting_verse :: integer, ending_verse :: integer) :: String.t()
  def verses(starting_verse, ending_verse) do
    starting_verse..ending_verse
    |> Enum.map(&verse/1)
    |> Enum.join("\n")
  end

  @doc """
  Sing all 12 verses, in order, one verse per line.
  """
  @spec sing() :: String.t()
  def sing do
    verses(1, 12)
  end
end
