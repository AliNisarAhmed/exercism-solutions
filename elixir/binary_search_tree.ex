defmodule BinarySearchTree do
  @type bst_node :: %{data: any, left: bst_node | nil, right: bst_node | nil}

  @doc """
  Create a new Binary Search Tree with root's value as the given 'data'
  """
  @spec new(any) :: bst_node
  def new(data) do
    %{data: data, left: nil, right: nil}
  end

  @doc """
  Creates and inserts a node with its value as 'data' into the tree.
  """
  @spec insert(bst_node, any) :: bst_node
  def insert(nil, val), do: new(val)

  def insert(%{data: data, right: right} = tree, val) when val > data do
    %{tree | right: insert(right, val)}
  end

  def insert(%{data: data, left: left} = tree, val) when val <= data do
    %{tree | left: insert(left, val)}
  end

  @doc """
  Traverses the Binary Search Tree in order and returns a list of each node's data.
  """
  # @spec in_order(bst_node) :: [any]
  # def in_order(nil), do: []
  # def in_order(%{data: data, left: nil, right: nil}), do: [data]
  # def in_order(%{data: data, left: left, right: right}) do
  #   in_order(left) ++ [data] ++ in_order(right)
  # end

  # Below is an O(n) solution, using an acc to avoid appending to list which is slowish
  @spec in_order(bst_node) :: [any]
  def in_order(root) do
    in_order(root, [])
    |> Enum.reverse()
  end

  defp in_order(nil, acc) do
    acc
  end

  defp in_order(%{data: data, left: nil, right: nil}, acc) do
    [data | acc]
  end

  defp in_order(%{data: data, left: left, right: right}, acc) do
    acc = in_order(left, acc)
    acc = [data | acc]
    in_order(right, acc)
  end
end
