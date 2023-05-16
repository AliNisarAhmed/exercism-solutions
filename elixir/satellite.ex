defmodule Satellite do
  @typedoc """
  A tree, which can be empty, or made from a left branch, a node and a right branch
  """
  @type tree :: {} | {tree, any, tree}

  @doc """
  Build a tree from the elements given in a pre-order and in-order style
  """
  @spec build_tree(preorder :: [any], inorder :: [any]) :: {:ok, tree} | {:error, String.t()}

  def build_tree(preorder, inorder) do
    with :ok <- validate_input(preorder, inorder) do
      {:ok, build_tree_from(preorder, inorder)}
    else
      e -> {:error, e}
    end
  end

  defp validate_input(preorder, inorder) when length(preorder) != length(inorder) do
    "traversals must have the same length"
  end

  defp validate_input(preorder, inorder) do
    set_preorder = MapSet.new(preorder)
    set_inorder = MapSet.new(inorder)

    if MapSet.size(set_preorder) != length(preorder) or
         MapSet.size(set_inorder) != length(inorder) do
      "traversals must contain unique items"
    else
      if MapSet.union(set_preorder, set_inorder) != set_inorder do
        "traversals must have the same elements"
      else
        :ok
      end
    end
  end

  defp build_tree_from([], []), do: leaf()

  defp build_tree_from([root | _rest_preorder], [root | _rest_inorder]) do
    {leaf(), root, leaf()}
  end

  defp build_tree_from([root, left, right | rest], inorder) do
    {left_inorder, right_inorder} = split_on(inorder, root)

    {
      build_tree_from([left, right | rest], left_inorder),
      root,
      build_tree_from([right | rest], right_inorder)
    }
  end

  defp leaf(), do: {}

  defp split_on(list, x) do
    {first, [_x | second]} = Enum.split_while(list, fn m -> m != x end)

    {first, second}
  end
end
