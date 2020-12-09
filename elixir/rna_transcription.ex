defmodule RnaTranscription do
  @doc """
  Transcribes a character list representing DNA nucleotides to RNA

  ## Examples

  iex> RnaTranscription.to_rna('ACTG')
  'UGAC'
  """

  @nucleotides %{?A => ?U, ?T => ?A, ?C => ?G, ?G => ?C}

  @spec to_rna([char]) :: [char]
  def to_rna(dna) do
    dna
    |> Enum.map(fn x -> @nucleotides[x] end)
  end
end
