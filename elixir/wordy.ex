defmodule Wordy do
  @doc """
  Calculate the math problem in the sentence.
  """
  @plus_op "plus"
  @minus_op "minus"
  @multiply_op "multiplied"
  @divide_op "divided"

  @spec answer(String.t()) :: integer
  def answer(question) do
    question
    |> parse_input()
    |> validate_input()
    |> convert_infix_to_postfix()
    |> eval_postfix()
  end

  defp parse_input(input) do
    [input] = String.split(input, "?", trim: true)

    case String.split(input, " ") do
      ["What", "is" | rest] -> Enum.filter(rest, fn x -> x != "by" end)
      _ -> raise ArgumentError
    end
  end

  defp validate_input([x]) do
    if is_operator?(x), do: raise(ArgumentError)
    [parse_int(x)]
  end

  defp validate_input([_x, _y]), do: raise(ArgumentError)

  defp validate_input(input) do
    for [x, y] <- Enum.chunk_every(input, 2, 1, :discard) do
      if is_operator?(x) and is_operator?(y), do: raise(ArgumentError)
      if not is_operator?(x) and not is_operator?(y), do: raise(ArgumentError)
    end

    input
    |> Enum.map(fn x ->
      if not is_operator?(x) do
        parse_int(x)
      else
        x
      end
    end)
  end

  defp parse_int(x), do: Integer.parse(x) |> elem(0)

  defp convert_infix_to_postfix(input) do
    convert(input, [], [])
  end

  defp convert([], [], acc), do: Enum.reverse(acc)
  defp convert([], operators, acc), do: Enum.reverse(operators ++ acc)

  defp convert([x | rest], [], acc) do
    if is_operator?(x) do
      convert(rest, [x], acc)
    else
      convert(rest, [], [x | acc])
    end
  end

  defp convert([x | rest], operators, acc) do
    cond do
      is_operator?(x) ->
        {remaining_operators, popped_operators} = pop_operators(operators, x)
        convert(rest, remaining_operators, popped_operators ++ acc)

      true ->
        convert(rest, operators, [x | acc])
    end
  end

  defp is_operator?(@plus_op), do: true
  defp is_operator?(@minus_op), do: true
  defp is_operator?(@multiply_op), do: true
  defp is_operator?(@divide_op), do: true
  defp is_operator?(_), do: false

  defp precedence(@plus_op), do: 1
  defp precedence(@minus_op), do: 1
  defp precedence(@multiply_op), do: 1
  defp precedence(@divide_op), do: 1

  defp pop_operators([], op), do: {[op], []}

  defp pop_operators([first_op | rest_op], op) do
    # https://www.geeksforgeeks.org/convert-infix-expression-to-postfix-expression/
    if precedence(op) <= precedence(first_op) do
      {remaining, popped} = pop_operators(rest_op, op)
      {remaining, Enum.reverse([first_op | popped])}
    else
      {[op, first_op | rest_op], []}
    end
  end

  defp eval_postfix(input), do: eval_postfix(input, [])

  defp eval_postfix([], [result | _rest_stack]), do: result

  defp eval_postfix([z | rest_input], []), do: eval_postfix(rest_input, [z])

  defp eval_postfix([z | rest_input], [x]), do: eval_postfix(rest_input, [z, x])

  defp eval_postfix([z | rest_input], [x, y | rest_stack] = stack) do
    if is_operator?(x), do: raise(ArgumentError)

    if is_operator?(z) do
      eval_postfix(rest_input, [eval(y, x, z) | rest_stack])
    else
      eval_postfix(rest_input, [z | stack])
    end
  end

  defp eval(x, y, @plus_op), do: x + y
  defp eval(x, y, @minus_op), do: x - y
  defp eval(x, y, @multiply_op), do: x * y
  defp eval(x, y, @divide_op), do: x / y
end
