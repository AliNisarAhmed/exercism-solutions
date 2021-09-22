defmodule RationalNumbers do
  @type rational :: {integer, integer}

  alias Math

  def make_rational(num, den) when (num > 0 and den < 0) or (num < 0 and den < 0) do 
    g = Math.gcd(num, den) 
    {-1 * div(num, g), -1 * div(den, g)}
  end

  def make_rational(num, den) do 
    g = Math.gcd(num, den)
    {div(num, g), div(den, g)}
  end

  def pow(a, n), do: :math.pow(a, n) |> round()

  @doc """
  Add two rational numbers
  """
  @spec add(a :: rational, b :: rational) :: rational
  def add({a1, b1}, {a2, b2}) do
    make_rational(a1 * b2 + a2 * b1, b1 * b2)
  end

  @doc """
  Subtract two rational numbers
  """
  @spec subtract(a :: rational, b :: rational) :: rational
  def subtract({a1, b1}, {a2, b2}) do
    make_rational(a1 * b2 - a2 * b1, b1 * b2)
  end

  @doc """
  Multiply two rational numbers
  """
  @spec multiply(a :: rational, b :: rational) :: rational
  def multiply({a1, b1}, {a2, b2}) do
    make_rational(a1 * a2, b1 * b2)
  end

  @doc """
  Divide two rational numbers
  """
  @spec divide_by(num :: rational, den :: rational) :: rational
  def divide_by({a1, b1}, {a2, b2}) when a2 != 0 do
    make_rational(a1 * b2, a2 * b1)
  end

  @doc """
  Absolute value of a rational number
  """
  @spec abs_rat(a :: rational) :: rational
  def abs_rat({a, b}) do
    make_rational(Kernel.abs(a), Kernel.abs(b)) 
  end

  @doc """
  Exponentiation of a rational number by an integer
  """
  @spec pow_rational(a :: rational, n :: integer) :: rational
  def pow_rational({a, b}, n) when n >= 0 do
    make_rational(pow(a, n), pow(b, n))
  end

  def pow_rational({a, b}, n) do 
    make_rational(pow(b, n), pow(a, n))
  end

  @doc """
  Exponentiation of a real number by a rational number
  """
  @spec pow_real(x :: integer, n :: rational) :: float
  def pow_real(x, {a, b}) do
    Math.nth_root(:math.pow(x, a), b)
  end

  @doc """
  Reduce a rational number to its lowest terms
  """
  @spec reduce(a :: rational) :: rational
  def reduce({a, b}) do
    make_rational(a, b)
  end
end
