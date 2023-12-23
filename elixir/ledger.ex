defmodule Ledger do
  @doc """
  Format the given entries given a currency and locale
  """
  @type currency :: :usd | :eur
  @type locale :: :en_US | :nl_NL
  @type entry :: %{amount_in_cents: integer(), date: Date.t(), description: String.t()}

  @spec format_entries(currency(), locale(), list(entry())) :: String.t()
  def format_entries(_currency, locale, []) do
    get_header(locale)
  end

  def format_entries(currency, locale, entries) do
    header = get_header(locale)

    entries =
      entries
      |> Enum.sort(&compare_entries/2)
      |> Enum.map(fn entry -> format_entry(currency, locale, entry) end)
      |> Enum.join("\n")

    header <> entries <> "\n"
  end

  @spec compare_entries(entry(), entry()) :: boolean()
  defp compare_entries(e1, e2) when e1.date.day < e2.date.day, do: true
  defp compare_entries(e1, e2) when e1.date.day > e2.date.day, do: false
  defp compare_entries(e1, e2) when e1.description < e2.description, do: true
  defp compare_entries(e1, e2) when e1.description > e2.description, do: false
  defp compare_entries(e1, e2), do: e1.amount_in_cents <= e2.amount_in_cents

  @spec get_header(locale()) :: String.t()
  defp get_header(:en_US) do
    "Date       | Description               | Change       \n"
  end

  defp get_header(_locale) do
    "Datum      | Omschrijving              | Verandering  \n"
  end

  @spec format_date(entry(), locale()) :: String.t()
  defp format_date(entry, :en_US) do
    format_month(entry) <> "/" <> format_day(entry) <> "/" <> format_year(entry) <> " "
  end

  defp format_date(entry, _locale) do
    format_day(entry) <> "-" <> format_month(entry) <> "-" <> format_year(entry) <> " "
  end

  @spec format_month(entry()) :: String.t()
  defp format_month(entry) do
    entry.date.month
    |> to_string()
    |> String.pad_leading(2, "0")
  end

  @spec format_day(entry()) :: String.t()
  defp format_day(entry) do
    entry.date.day
    |> to_string()
    |> String.pad_leading(2, "0")
  end

  @spec format_year(entry()) :: String.t()
  defp format_year(entry), do: to_string(entry.date.year)

  @spec format_amount_in_decimal(entry()) :: String.t()
  defp format_amount_in_decimal(entry) do
    entry.amount_in_cents
    |> abs
    |> rem(100)
    |> to_string()
    |> String.pad_leading(2, "0")
  end

  @spec format_amount_in_whole(entry(), locale()) :: String.t()
  defp format_amount_in_whole(entry, locale) do
    amount =
      entry.amount_in_cents
      |> div(100)
      |> abs()

    if amount < 1000 do
      to_string(amount)
    else
      div =
        amount
        |> div(1000)
        |> to_string()

      rem =
        amount
        |> rem(1000)
        |> to_string()

      case locale do
        :en_US -> div <> "," <> rem
        _ -> div <> "." <> rem
      end
    end
  end

  @spec format_number(entry(), locale()) :: String.t()
  defp format_number(entry, locale) do
    decimal = format_amount_in_decimal(entry)
    whole = format_amount_in_whole(entry, locale)

    case locale do
      :en_US -> whole <> "." <> decimal
      _ -> whole <> "," <> decimal
    end
  end

  @spec currency_symbol(currency()) :: String.t()
  defp currency_symbol(:eur), do: "€"
  defp currency_symbol(:usd), do: "$"

  @spec format_amount(currency(), entry(), String.t(), locale()) :: String.t()
  def format_amount(currency, entry, number, :en_US) when entry.amount_in_cents >= 0 do
    "  #{currency_symbol(currency)}#{number} "
  end

  def format_amount(currency, entry, number, _locale) when entry.amount_in_cents >= 0 do
    " #{currency_symbol(currency)} #{number} "
  end

  def format_amount(currency, _entry, number, :en_US) do
    " (#{currency_symbol(currency)}#{number})"
  end

  def format_amount(currency, _entry, number, _locale) do
    " #{currency_symbol(currency)} -#{number} "
  end

  defp format_description(entry) do
    if String.length(entry.description) > 26 do
      " " <> String.slice(entry.description, 0, 22) <> "..."
    else
      " " <> String.pad_trailing(entry.description, 25, " ")
    end
  end

  @spec format_entry(currency(), locale(), entry()) :: String.t()
  defp format_entry(currency, locale, entry) do
    date = format_date(entry, locale)

    number = format_number(entry, locale)

    amount =
      format_amount(currency, entry, number, locale)
      |> String.pad_leading(14, " ")

    description = format_description(entry)

    date <> "|" <> description <> " |" <> amount
  end
end
