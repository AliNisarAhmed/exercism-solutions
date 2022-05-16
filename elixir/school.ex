defmodule School do
  @moduledoc """
  Simulate students in a school.

  Each student is in a grade.
  """

  @type school :: %{String.t() => integer()}

  @doc """
  Create a new, empty school.
  """
  @spec new() :: school
  def new() do
    %{}
  end

  @doc """
  Add a student to a particular grade in school.
  """
  @spec add(school, String.t(), integer) :: {:ok | :error, school}
  def add(school, name, grade) do
    if Map.has_key?(school, name) do
      {:error, school}
    else
      school =
        school
        |> Map.put_new(name, grade)

      {:ok, school}
    end
  end

  @doc """
  Return the names of the students in a particular grade, sorted alphabetically.
  """
  @spec grade(school, integer) :: [String.t()]
  def grade(school, grade) do
    school
    |> Enum.filter(fn {_k, v} -> v == grade end)
    |> Enum.map(fn {k, _v} -> k end)
  end

  @doc """
  Return the names of all the students in the school sorted by grade and name.
  """
  @spec roster(school) :: [String.t()]
  def roster(school) do
    school
    |> Enum.sort_by(fn {name, grade} -> {grade, name} end)
    |> Enum.map(fn {name, _grade} -> name end)
  end
end
