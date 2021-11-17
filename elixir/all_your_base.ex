defmodule AllYourBase do
  @doc """
  Given a number in input base, represented as a sequence of digits, converts it to output base,
  or returns an error tuple if either of the bases are less than 2
  """

  @spec convert(list, integer, integer) :: {:ok, list} | {:error, String.t()}
  def convert(_digits, _input_base, output_base) when output_base < 2 do
    {:error, "output base must be >= 2"}
  end

  def convert(_digits, input_base, _output_base) when input_base < 2 do
    {:error, "input base must be >= 2"}
  end

  def convert(digits, input_base, 10) do
    if check_neg_or_greater_than_base?(digits, input_base) do
      {:error, "all digits must be >= 0 and < input base"}
    else
      {:ok, number_to_base_10(digits, input_base)}
    end
  end

  def convert(digits, input_base, output_base) do
    if check_neg_or_greater_than_base?(digits, input_base) do
      {:error, "all digits must be >= 0 and < input base"}
    else
      {:ok,
       digits
       |> number_to_base_10(input_base)
       |> Integer.undigits()
       |> base_10_to_other(output_base)}
    end
  end

  # ------------------ PRIVATE -----------------------

  defp check_neg_or_greater_than_base?(digits, input_base) do
    Enum.any?(digits, &(&1 < 0 or &1 >= input_base))
  end

  defp base_10_to_other(base_10, output_base) do
    base_10_to_other(base_10, output_base, [])
  end

  defp base_10_to_other(base_10, output_base, acc) do
    remainder = :math.fmod(base_10, output_base)
    quotient = div(base_10, output_base)

    if quotient == 0 do
      [remainder | acc]
    else
      base_10_to_other(quotient, output_base, [remainder | acc])
    end
  end

  defp number_to_base_10(digits, input_base) do
    max_pow = length(digits) - 1

    range =
      max_pow..0
      |> Enum.to_list()

    [range, digits]
    |> Enum.zip_with(fn [exp, digit] ->
      input_base
      |> :math.pow(exp)
      |> Kernel.*(digit)
      |> trunc()
    end)
    |> Enum.sum()
    |> Integer.digits()
  end
end
