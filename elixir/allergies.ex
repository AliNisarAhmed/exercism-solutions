defmodule Allergies do
  @doc """
  List the allergies for which the corresponding flag bit is true.
  """
  @spec list(non_neg_integer) :: [String.t()]
  def list(flags) do
    Stream.unfold(flags, &determine_allergy/1)
    |> Enum.to_list()
  end

  def determine_allergy(n) when Bitwise.band(n, 128) == 128 do
    {"cats", n - 128}
  end

  def determine_allergy(n) when Bitwise.band(n, 64) == 64 do
    {"pollen", n - 64}
  end

  def determine_allergy(n) when Bitwise.band(n, 32) == 32 do
    {"chocolate", n - 32}
  end

  def determine_allergy(n) when Bitwise.band(n, 16) == 16 do
    {"tomatoes", n - 16}
  end

  def determine_allergy(n) when Bitwise.band(n, 8) == 8 do
    {"strawberries", n - 8}
  end

  def determine_allergy(n) when Bitwise.band(n, 4) == 4 do
    {"shellfish", n - 4}
  end

  def determine_allergy(n) when Bitwise.band(n, 2) == 2 do
    {"peanuts", n - 2}
  end

  def determine_allergy(n) when Bitwise.band(n, 1) == 1 do
    {"eggs", n - 1}
  end

  def determine_allergy(_), do: nil

  @doc """
  Returns whether the corresponding flag bit in 'flags' is set for the item.
  """
  @spec allergic_to?(non_neg_integer, String.t()) :: boolean
  def allergic_to?(flags, item) do
    item in list(flags)
  end
end
