defmodule RobotSimulator do
  @type robot() :: %{direction: direction(), position: position()}
  @type direction() :: :north | :east | :south | :west
  @type position() :: {integer(), integer()}

  @doc """
  Create a Robot Simulator given an initial direction and position.

  Valid directions are: `:north`, `:east`, `:south`, `:west`
  """
  @spec create(direction, position) :: robot() | {:error, String.t()}
  def create(direction, _pos) when direction not in [:north, :south, :east, :west] do
    {:error, "invalid direction"}
  end

  def create(direction, {x, y} = position) when is_integer(x) and is_integer(y) do
    %{direction: direction, position: position}
  end

  def create(_direction, _pos) do
    {:error, "invalid position"}
  end

  def create(), do: %{direction: :north, position: {0, 0}}

  @doc """
  Simulate the robot's movement given a string of instructions.

  Valid instructions are: "R" (turn right), "L", (turn left), and "A" (advance)
  """
  @spec simulate(robot, instructions :: String.t()) :: robot() | {:error, String.t()}
  def simulate(robot, ""), do: robot

  def simulate(%{direction: direction, position: position}, "R" <> rest) do
    direction
    |> turn_right()
    |> create(position)
    |> simulate(rest)
  end

  def simulate(%{direction: direction, position: position}, "L" <> rest) do
    direction
    |> turn_left()
    |> create(position)
    |> simulate(rest)
  end

  def simulate(%{direction: direction, position: position}, "A" <> rest) do
    position
    |> advance(direction)
    |> then(&create(direction, &1))
    |> simulate(rest)
  end

  def simulate(_dir, _pos), do: {:error, "invalid instruction"}

  @doc """
  Return the robot's direction.

  Valid directions are: `:north`, `:east`, `:south`, `:west`
  """
  @spec direction(robot) :: direction()
  def direction(%{direction: direction}) do
    direction
  end

  @doc """
  Return the robot's position.
  """
  @spec position(robot) :: position()
  def position(%{position: position}) do
    position
  end

  @spec turn_right(direction()) :: direction()
  defp turn_right(direction) do
    case direction do
      :north -> :east
      :east -> :south
      :south -> :west
      :west -> :north
    end
  end

  @spec turn_left(direction()) :: direction()
  defp turn_left(direction) do
    case direction do
      :north -> :west
      :west -> :south
      :south -> :east
      :east -> :north
    end
  end

  @spec advance(position, direction) :: position
  defp advance({x, y}, direction) do
    case direction do
      :north -> {x, y + 1}
      :south -> {x, y - 1}
      :east -> {x + 1, y}
      :west -> {x - 1, y}
    end
  end
end
