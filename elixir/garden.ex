defmodule Garden do
  @type plant :: :clover | :radishes | :violets | :grass
  @students [
    :alice,
    :bob,
    :charlie,
    :david,
    :eve,
    :fred,
    :ginny,
    :harriet,
    :ileana,
    :joseph,
    :kincaid,
    :larry
  ]
  @doc """
    Accepts a string representing the arrangement of cups on a windowsill and a
    list with names of students in the class. The student names list does not
    have to be in alphabetical order.

    It decodes that string into the various gardens for each student and returns
    that information in a map.
  """

  @spec info(String.t(), list) :: map
  def info(info_string, student_names \\ @students) do
    student_names =
      student_names
      |> Enum.sort_by(&to_string/1)

    info_string
    |> parse()
    |> zip_by_students(student_names)
    |> Enum.map(&Map.new/1)
    |> merge_rows()
  end

  @spec parse(String.t(), integer()) :: list(plant)
  defp parse(str, number_of_students \\ 12) do
    str
    |> String.split("\n", trim: true)
    |> Enum.map(fn s ->
      s
      |> String.pad_trailing(number_of_students)
      |> String.graphemes()
      |> Enum.map(&parse_plant/1)
    end)
  end

  @spec parse_plant(String.t()) :: plant | nil
  defp parse_plant(string) do
    case string do
      "R" -> :radishes
      "C" -> :clover
      "G" -> :grass
      "V" -> :violets
      _ -> nil
    end
  end

  defp zip_by_students(list, student_names) do
    list
    |> Enum.map(fn sub_list ->
      sub_list
      |> Enum.chunk_every(2)
      |> Enum.zip_with(student_names, fn ls, student -> {student, ls} end)
    end)
  end

  @spec merge_rows(list(map)) :: map
  defp merge_rows([map1, map2]) do
    map1
    |> Map.merge(map2, fn _k, v1, v2 ->
      v1
      |> Enum.concat(v2)
      |> make_garden_tuple()
    end)
  end

  defp make_garden_tuple([nil, nil, nil, nil]), do: {}

  defp make_garden_tuple(list) do
    list
    |> List.to_tuple()
  end
end
