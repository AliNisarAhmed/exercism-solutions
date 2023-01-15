defmodule Grep do
  @spec grep(String.t(), [String.t()], [String.t()]) :: String.t()
  def grep(pattern, flags, [_file_name] = file_names) do
    options = parse_flags(flags) ++ [multiple_files: false]
    process_files(file_names, pattern, options)
  end

  def grep(pattern, flags, file_names) do
    options = parse_flags(flags) ++ [multiple_files: true]
    process_files(file_names, pattern, options)
  end

  def process_files(file_names, pattern, options) do
    file_names
    |> Enum.map(&process_file(&1, pattern, options))
    |> Enum.join()
  end

  def process_file(file_name, pattern, options) do
    file_name
    |> File.stream!()
    |> Stream.with_index(1)
    |> process_lines(pattern, file_name, options)
    |> remove_dups_if_multi_files(multiple_files: Keyword.get(options, :multiple_files, false))
    |> Enum.join()
  end

  def process_lines(lines, pattern, file_name, options) do
    lines
    |> match_pattern(pattern, options)
    |> print_lines(file_name, options)
  end

  # ------------------------------------------------------------------------------
  def transform_line({original_text, number}, case_insensitive: true),
    do:
      {original_text,
       original_text
       |> String.trim()
       |> String.downcase(), number}

  def transform_line({original_text, number}, case_insensitive: false),
    do: {original_text, String.trim(original_text), number}

  # ------------------------------------------------------------------------------
  def match_pattern(lines, pattern, options \\ []) do
    case_insensitive? = Keyword.get(options, :case_insensitive, false)
    entire_line? = Keyword.get(options, :entire_line, false)
    invert? = Keyword.get(options, :invert, false)

    lines
    |> Enum.map(&transform_line(&1, case_insensitive: case_insensitive?))
    |> Enum.filter(
      &if invert? do
        not match(&1, pattern, case_insensitive: case_insensitive?, entire_line: entire_line?)
      else
        match(&1, pattern, case_insensitive: case_insensitive?, entire_line: entire_line?)
      end
    )
  end

  def match(line, pattern, [{:case_insensitive, true} | rest]) do
    match(line, String.downcase(pattern), rest)
  end

  def match(line, pattern, [{:case_insensitive, false} | rest]) do
    match(line, pattern, rest)
  end

  def match({_original, text, _number}, pattern, entire_line: false),
    do: String.contains?(text, pattern)

  def match({_original, text, _number}, pattern, entire_line: true), do: text == pattern

  # ------------------------------------------------------------------------------
  def print_lines(lines, file_name, options) do
    print_file_name? = Keyword.get(options, :print_file_name, false)
    prefix_line_no? = Keyword.get(options, :prefix_line_no, false)
    multiple_files? = Keyword.get(options, :multiple_files, false)

    lines
    |> Enum.map(
      &print_line(&1, file_name,
        print_file_name: print_file_name?,
        prefix_line_no: prefix_line_no?,
        multiple_files: multiple_files?
      )
    )
  end

  def print_line(_line, file_name, [{:print_file_name, true} | _rest]), do: file_name <> "\n"

  def print_line(line, file_name, [{:print_file_name, false} | rest]) do
    print_line(line, file_name, rest)
  end

  def print_line({original, _text, number}, file_name, [{:prefix_line_no, false} | rest]) do
    print_line({original, original, number}, file_name, rest)
  end

  def print_line({original, _text, number}, file_name, [{:prefix_line_no, true} | rest]) do
    print_line({original, "#{number}:#{original}", number}, file_name, rest)
  end

  def print_line({_original, text, _number}, _file_name, [{:multiple_files, false} | _rest]) do
    text
  end

  def print_line({_original, text, _number}, file_name, [{:multiple_files, true} | _rest]) do
    "#{file_name}:#{text}"
  end

  # ------------------------------------------------------------------------------
  def remove_dups_if_multi_files(lines, multiple_files: true), do: Enum.uniq(lines)
  def remove_dups_if_multi_files(lines, multiple_files: false), do: lines

  # ------------------------------------------------------------------------------
  def parse_flags(flags) do
    flags
    |> Enum.reduce([], fn
      "-n", acc -> [{:prefix_line_no, true} | acc]
      "-l", acc -> [{:print_file_name, true} | acc]
      "-v", acc -> [{:invert, true} | acc]
      "-i", acc -> [{:case_insensitive, true} | acc]
      "-x", acc -> [{:entire_line, true} | acc]
    end)
  end
end
