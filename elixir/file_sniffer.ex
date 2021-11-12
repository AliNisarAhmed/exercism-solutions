defmodule FileSniffer do
  def type_from_extension("exe"), do: "application/octet-stream"
  def type_from_extension("bmp"), do: "image/bmp"
  def type_from_extension("png"), do: "image/png"
  def type_from_extension("jpg"), do: "image/jpg"
  def type_from_extension(_), do: "image/gif"

  def type_from_binary(<<66, 77, 30, _rest::binary>>) do
    type_from_extension("bmp")
  end

  def type_from_binary(<<71, 73, 70, _rest::binary>>) do
    type_from_extension("gif")
  end

  def type_from_binary(<<255, 216, 255, _rest::binary>>) do
    type_from_extension("jpg")
  end

  def type_from_binary(<<137, 80, 78, _rest::binary>>) do
    type_from_extension("png")
  end

  def type_from_binary(<<127, 69, 76, _rest::binary>>) do
    type_from_extension("exe")
  end

  def verify(file_binary, extension) do
    binary = type_from_binary(file_binary)
    ext = type_from_extension(extension)

    if binary == ext do
      {:ok, ext}
    else
      {:error, "Warning, file format and file extension do not match."}
    end
  end
end
