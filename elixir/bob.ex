defmodule Bob do
  @spec hey(String.t()) :: String.t()
  def hey(input) do
    input = String.trim(input) 
    is_question = is_question?(input)
    is_empty = is_empty?(input)
    is_yelled = is_upper?(input)
    cond do 
      is_question and is_yelled -> "Calm down, I know what I'm doing!"
      is_question -> "Sure."
      not is_empty and is_yelled -> "Whoa, chill out!"
      is_empty?(input) -> "Fine. Be that way!"
      true -> "Whatever."
    end

  end


  def is_question?(str) do 
    String.last(str) == "?"
  end

  def is_upper?(str) do 
    filtered = remove_non_alpha(str)
    not is_empty?(filtered) and String.upcase(filtered) == filtered
  end 

  def is_empty?(str) do 
    str
    |> String.trim
    |> String.length 
    |> Kernel.==(0)
  end

  def remove_non_alpha(str) do 
    str 
    |> String.replace(~r/[^[:alpha:]]/, "")
  end

end
