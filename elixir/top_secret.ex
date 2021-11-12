defmodule TopSecret do
  def to_ast(string) do
    with {:ok, quoted_form} <- Code.string_to_quoted(string) do
      quoted_form
    end
  end

  def decode_secret_message_part(
        {_, _, [{_name, _, args} | _rest]} = ast_node,
        acc
      )
      when is_nil(args) or (is_list(args) and length(args) == 0) do
    {ast_node, ["" | acc]}
  end

  def decode_secret_message_part(
        {keyword, _,
         [
           {:when, _, [{name, _, args} | _rest]},
           _
         ]} = ast_node,
        acc
      )
      when keyword in [:def, :defp] do
    trucated_name =
      name
      |> to_string
      |> String.slice(0, length(args))

    {ast_node, [trucated_name | acc]}
  end

  def decode_secret_message_part(
        {keyword, _, [{name, _, args} | _rest]} = ast_node,
        acc
      )
      when keyword in [:def, :defp] do
    trucated_name =
      name
      |> to_string
      |> String.slice(0, length(args))

    {ast_node, [trucated_name | acc]}
  end

  def decode_secret_message_part(ast_node, acc), do: {ast_node, acc}

  def decode_secret_message(string) do
    string
    |> to_ast()
    |> Macro.prewalk([], &decode_secret_message_part/2)
    |> elem(1)
    |> Enum.reverse()
    |> Enum.join()
  end
end
