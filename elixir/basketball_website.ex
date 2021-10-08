defmodule BasketballWebsite do

  def extract_from_path(data, path) do
    extract(data, String.split(path, "."))
  end

  def extract(nil, _), do: nil 

  def extract(data, []), do: data 

  def extract(data, [current | rest]) do 
    extract(data[current], rest)
  end

  def get_in_path(data, path) do
    Kernel.get_in(data, String.split(path, "."))
  end
end
