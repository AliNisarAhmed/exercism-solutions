defmodule Username do
  @special_chars %{?ä => 'ae', ?ö => 'oe', ?ü => 'ue', ?ß => 'ss'}
  # def sanitize(username) do
  #   # ä becomes ae
  #   # ö becomes oe
  #   # ü becomes ue
  #   # ß becomes ss

  def sanitize([c | cs]) do
    is_special = Map.has_key?(@special_chars, c)
    case c do
      c when c >= ?a and c <= ?z -> [c | sanitize(cs)]
      c when c == ?_ -> [c | sanitize(cs)]
      c when is_special -> Map.get(@special_chars, c) ++ sanitize(cs)
      _ -> sanitize(cs)
    end
  end

  def sanitize([]), do: []
end
