defmodule GuessingGame do
  def compare(secret_number, guess \\ :no_guess)
  def compare(secret_number, guess) when is_integer(guess) do
    cond do 
      secret_number == guess -> "Correct" 
      abs(secret_number - guess) == 1 -> "So close"
      guess > secret_number -> "Too high"
      true -> "Too low"
    end
  end

  def compare(_s, :no_guess) do 
    "Make a guess"
  end
end
