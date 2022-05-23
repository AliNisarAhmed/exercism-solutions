defmodule DndCharacter do
  @type t :: %__MODULE__{
          strength: pos_integer(),
          dexterity: pos_integer(),
          constitution: pos_integer(),
          intelligence: pos_integer(),
          wisdom: pos_integer(),
          charisma: pos_integer(),
          hitpoints: pos_integer()
        }

  defstruct ~w[strength dexterity constitution intelligence wisdom charisma hitpoints]a

  @spec modifier(pos_integer()) :: integer()
  def modifier(score) do
    score
    |> Kernel.-(10)
    |> Kernel./(2)
    |> Float.floor()
    |> trunc()
  end

  @spec ability :: pos_integer()
  def ability do
    0..3
    |> Enum.map(fn _ -> :rand.uniform(6) end)
    |> Enum.sort()
    |> Enum.take(3)
    |> Enum.sum()
  end

  @spec character :: t()
  def character do
    constitution = ability()
    c_modifier = modifier(constitution)

    %__MODULE__{
      strength: ability(),
      dexterity: ability(),
      constitution: constitution,
      intelligence: ability(),
      wisdom: ability(),
      charisma: ability(),
      hitpoints: 10 + c_modifier
    }
  end
end
