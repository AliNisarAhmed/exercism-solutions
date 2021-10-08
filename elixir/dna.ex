defmodule DNA do
  @nuc_to_binary %{
    ?A => 0b0001,
    ?C => 0b0010,
    ?G => 0b0100,
    ?T => 0b1000,
    ?\s => 0b0000
  }

  @binary_to_nuc %{
    0b0001 => ?A,
    0b0010 => ?C, 
    0b0100 => ?G, 
    0b1000 => ?T, 
    0b0000 => ?\s
  }

  def encode_nucleotide(code_point) do
    Map.get(@nuc_to_binary, code_point)
  end

  def decode_nucleotide(encoded_code) do
    Map.get(@binary_to_nuc, encoded_code)
  end

  def encode([head | rest]) do 
    <<(<<encode_nucleotide(head)::4>>)::bitstring, encode(rest)::bitstring>>
  end

  def encode([]) do
    <<>>
  end

  def decode("") do 
    ''
  end

  def decode(<<value::4, rest::bitstring>>) do
    [decode_nucleotide(value)] ++ decode(rest)
  end
end
