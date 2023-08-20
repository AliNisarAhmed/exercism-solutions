defmodule PhoneNumber do
  @doc """
  Remove formatting from a phone number if the given number is valid. Return an error otherwise.
  """

  @format ~r/^[+]?[1]?[\s\-\.(]*(?<area_code>\S\S\S)[\s\-\.()]*(?<exchange_code>\S\S\S)[\s\-\.()]*(?<subscriber_num>[\d]+)[\s\-\.()]*/
  @three_digits ~r/\d\d\d/

  @spec clean(String.t()) :: {:ok, String.t()} | {:error, String.t()}
  def clean(raw) do
    with %{
           "area_code" => ac,
           "exchange_code" => ec,
           "subscriber_num" => sn
         } <- Regex.named_captures(@format, raw),
         :ok <- validate_length(ac, ec, sn),
         {:ok, area_code} <- validate_area_code(ac),
         {:ok, exchange_code} <- validate_exchange_code(ec) do
      {:ok, area_code <> exchange_code <> sn}
    else
      nil -> {:error, "incorrect number of digits"}
      e -> e
    end
  end

  def validate_length(ac, ec, sn) do
    number = ac <> ec <> sn
    len = String.length(number)

    if len == 11 and not String.starts_with?(number, "1") do
      {:error, "11 digits must start with 1"}
    else
      if len != 10 do
        nil
      else
        :ok
      end
    end
  end

  def validate_area_code("0" <> _rest) do
    {:error, "area code cannot start with zero"}
  end

  def validate_area_code("1" <> _rest) do
    {:error, "area code cannot start with one"}
  end

  def validate_area_code(area_code) do
    validate_three_digits(area_code)
  end

  def validate_exchange_code("0" <> _rest) do
    {:error, "exchange code cannot start with zero"}
  end

  def validate_exchange_code("1" <> _rest) do
    {:error, "exchange code cannot start with one"}
  end

  def validate_exchange_code(exchange_code) do
    validate_three_digits(exchange_code)
  end

  def validate_three_digits(number) do
    if Regex.match?(@three_digits, number) do
      {:ok, number}
    else
      {:error, "must contain digits only"}
    end
  end
end
