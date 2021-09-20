defmodule LanguageList do
  def new() do
    []
  end

  def add(list, language) do
    [language | list]
  end

  def remove([_ | rest]) do
    rest
  end

  def first([x | _]) do
    x
  end

  def count([_ | rest]) do
    1 + count(rest)
  end

  def count([]), do: 0

  def exciting_list?(list) do
    "Elixir" in list
  end
end
