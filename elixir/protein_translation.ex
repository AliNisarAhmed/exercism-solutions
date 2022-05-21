defmodule ProteinTranslation do
  @cysteine "Cysteine"
  @leucine "Leucine"
  @methionine "Methionine"
  @serine "Serine"
  @phenylalanine "Phenylalanine"
  @tryptophan "Tryptophan"
  @tyrosine "Tyrosine"
  @stop "STOP"

  @doc """
  Given an RNA string, return a list of proteins specified by codons, in order.
  """
  @spec of_rna(String.t()) :: {:ok, list(String.t())} | {:error, String.t()}
  def of_rna(""), do: {:ok, []}

  def of_rna(rna) do
    result =
      rna
      |> String.graphemes()
      |> Enum.chunk_every(3, 3)
      |> Enum.map(fn ls ->
        ls
        |> Enum.join()
        |> of_codon()
      end)
      |> Enum.reduce_while([], &accumulate_codons/2)

    with _ when is_list(result) <- result do
      {:ok, Enum.reverse(result)}
    else
      _ -> {:error, "invalid RNA"}
    end
  end

  defp accumulate_codons({:ok, @stop}, acc), do: {:halt, acc}
  defp accumulate_codons({:ok, codon}, acc), do: {:cont, [codon | acc]}
  defp accumulate_codons({:error, _} = e, _), do: {:halt, e}

  @doc """
  Given a codon, return the corresponding protein

  UGU -> Cysteine
  UGC -> Cysteine
  UUA -> Leucine
  UUG -> Leucine
  AUG -> Methionine
  UUU -> Phenylalanine
  UUC -> Phenylalanine
  UCU -> Serine
  UCC -> Serine
  UCA -> Serine
  UCG -> Serine
  UGG -> Tryptophan
  UAU -> Tyrosine
  UAC -> Tyrosine
  UAA -> STOP
  UAG -> STOP
  UGA -> STOP
  """
  @spec of_codon(String.t()) :: {:ok, String.t()} | {:error, String.t()}
  def of_codon(s) when s in ["UGU", "UGC"], do: {:ok, @cysteine}
  def of_codon(s) when s in ["UUA", "UUG"], do: {:ok, @leucine}
  def of_codon(s) when s in ["AUG"], do: {:ok, @methionine}
  def of_codon(s) when s in ["UUU", "UUC"], do: {:ok, @phenylalanine}
  def of_codon(s) when s in ["UCU", "UCC", "UCA", "UCG"], do: {:ok, @serine}
  def of_codon(s) when s in ["UGG"], do: {:ok, @tryptophan}
  def of_codon(s) when s in ["UAU", "UAC"], do: {:ok, @tyrosine}
  def of_codon(s) when s in ["UAA", "UAG", "UGA"], do: {:ok, @stop}
  def of_codon(_), do: {:error, "invalid codon"}
end
