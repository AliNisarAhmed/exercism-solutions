defmodule ComplexNumbers do
  @typedoc """
  In this module, complex numbers are represented as a tuple-pair containing the real and
  imaginary parts.
  For example, the real number `1` is `{1, 0}`, the imaginary number `i` is `{0, 1}` and
  the complex number `4+3i` is `{4, 3}'.
  """
  @type complex :: {float, float}

  @doc """
  Return the real part of a complex number
  """
  @spec real(a :: complex) :: float
  def real({real, _imaginary}), do: real

  @doc """
  Return the imaginary part of a complex number
  """
  @spec imaginary(a :: complex) :: float
  def imaginary({_real, imaginary}), do: imaginary

  @doc """
  Multiply two complex numbers, or a real and a complex number
  """
  @spec mul(a :: complex | float, b :: complex | float) :: complex
  def mul({r1, i1}, {r2, i2}) do
    {r1 * r2 - i1 * i2, r1 * i2 + r2 * i1}
  end

  def mul({_r1, _i1} = a, b), do: mul(a, {b, 0.0})
  def mul(a, {_r2, _i2} = b), do: mul({a, 0.0}, b)
  def mul(a, b), do: mul({a, 0.0}, {b, 0.0})

  @doc """
  Add two complex numbers, or a real and a complex number
  """
  @spec add(a :: complex | float, b :: complex | float) :: complex
  def add({r1, i1}, {r2, i2}) do
    {r1 + r2, i1 + i2}
  end

  def add({_r1, _i1} = a, b), do: add(a, {b, 0.0})
  def add(a, {_r2, _i2} = b), do: add({a, 0.0}, b)
  def add(a, b), do: add({a, 0.0}, {b, 0.0})

  @doc """
  Subtract two complex numbers, or a real and a complex number
  """
  @spec sub(a :: complex | float, b :: complex | float) :: complex
  def sub(a, b) do
    add(a, mul(b, {-1.0, 0.0}))
  end

  @doc """
  Divide two complex numbers, or a real and a complex number
  """
  @spec div(a :: complex | float, b :: complex | float) :: complex
  def div({a, b}, {c, d}) do
    {(a * c + b * d) / (c ** 2 + d ** 2), (b * c - a * d) / (c ** 2 + d ** 2)}
  end

  def div({_r, _i} = a, b), do: ComplexNumbers.div(a, {b, 0.0})
  def div(a, {_r, _i} = b), do: ComplexNumbers.div({a, 0.0}, b)

  @doc """
  Absolute value of a complex number
  """
  @spec abs(a :: complex) :: float
  def abs({r, i}) do
    (r ** 2 + i ** 2)
    |> :math.sqrt()
  end

  @doc """
  Conjugate of a complex number
  """
  @spec conjugate(a :: complex) :: complex
  def conjugate({r, i}), do: {r, -1 * i}

  @doc """
  Exponential of a complex number
  """
  @spec exp(a :: complex) :: complex
  def exp({r, i}) do
    exp_real = :math.exp(r)

    {exp_real * :math.cos(i), exp_real * :math.sin(i)}
  end
end
