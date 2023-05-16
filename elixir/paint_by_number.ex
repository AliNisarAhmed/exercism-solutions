defmodule PaintByNumber do
  def palette_bit_size(color_count) do
    # Please implement the palette_bit_size/1 function
    :math.ceil(:math.log2(color_count)) |> round()
  end

  def empty_picture() do
    <<>>
  end

  def test_picture() do
    <<0::2, 1::2, 2::2, 3::2>>
  end

  def prepend_pixel(picture, color_count, pixel_color_index) do
    <<pixel_color_index::size(palette_bit_size(color_count)), picture::bitstring>>
  end

  def get_first_pixel(<<>>, _color_count), do: nil

  def get_first_pixel(picture, color_count) do
    size = palette_bit_size(color_count)
    <<value::size(size), _rest::bitstring>> = picture
    value
  end

  def drop_first_pixel(<<>>, _color_count), do: <<>>

  def drop_first_pixel(picture, color_count) do
    size = palette_bit_size(color_count)
    <<_value::size(size), rest::bitstring>> = picture
    rest
  end

  def concat_pictures(picture1, picture2) do
    <<picture1::bitstring, picture2::bitstring>>
  end
end
