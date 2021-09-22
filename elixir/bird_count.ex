defmodule BirdCount do
  def today([today | _rest]) do
    today 
  end
  def today([]), do: nil

  def increment_day_count([today | rest]) do
    [today + 1 | rest]
  end
  def increment_day_count([]), do: [1]

  def has_day_without_birds?([x | _rest]) when x == 0, do: true
  def has_day_without_birds?([_x | rest]), do: has_day_without_birds?(rest)
  def has_day_without_birds?([]), do: false

  def total([x | xs]), do: x + total(xs)
  def total([]), do: 0

  def busy_days([x | xs]) when x >= 5, do: 1 + busy_days(xs)
  def busy_days([x | xs]) when x < 5, do: busy_days(xs)
  def busy_days([]), do: 0

end
