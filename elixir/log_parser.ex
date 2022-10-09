defmodule LogParser do
  def valid_line?(line) do
    Regex.match?(~r/^\[(DEBUG|INFO|WARNING|ERROR)\].*/, line)
  end

  def split_line(line) do
    String.split(line, ~r/<([~=*\-]*)>/)
  end

  def remove_artifacts(line) do
    String.replace(line, ~r/end-of-line[\d]+/i, "")
  end

  def tag_with_user_name(line) do
    with [_, username] <- Regex.run(~r/User\s+([\S]+).*/, line) do
      "[USER] #{username} " <> line
    else
      _ -> line
    end
  end
end
