defmodule House do
  @doc """
  Return verses of the nursery rhyme 'This is the House that Jack Built'.
  """

  @pairs [
    {"house that Jack built", "lay in"},
    {"malt", "ate"},
    {"rat", "killed"},
    {"cat", "worried"},
    {"dog", "tossed"},
    {"cow with the crumpled horn", "milked"},
    {"maiden all forlorn", "kissed"},
    {"man all tattered and torn", "married"},
    {"priest all shaven and shorn", "woke"},
    {"rooster that crowed in the morn", "kept"},
    {"farmer sowing his corn", "belonged to"},
    {"horse and the hound and the horn", ""}
  ]

  @spec recite(start :: integer, stop :: integer) :: String.t()
  def recite(start, stop) do
    start..stop
    |> Enum.map(&recite_verse/1)
    |> Enum.join("\n")
    |> then(&(&1 <> "\n"))
  end

  def recite_verse(n) do
    @pairs
    |> Enum.take(n)
    |> Enum.reverse()
    |> generate_verse()
  end

  def generate_verse(list) do
    first =
      list
      |> List.first()
      |> then(fn {noun, _verb} -> "This is the #{noun}" end)

    rest =
      list
      |> Enum.drop(1)
      |> Enum.map(fn {noun, verb} -> "that #{verb} the #{noun}" end)

    [first | rest]
    |> Enum.join(" ")
    |> then(&(&1 <> "."))
  end
end
