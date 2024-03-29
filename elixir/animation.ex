defmodule DancingDots.Animation do
  @type dot :: DancingDots.Dot.t()
  @type opts :: keyword
  @type error :: any
  @type frame_number :: pos_integer

  defmacro __using__(_opts) do
    quote do
      @behaviour DancingDots.Animation

      def init(opts) do
        {:ok, opts}
      end

      defoverridable init: 1
    end
  end

  @callback init(opts) :: {:ok, opts} | {:error, error}

  @callback handle_frame(dot, frame_number, opts) :: dot
end

defmodule DancingDots.Flicker do
  use DancingDots.Animation

  @impl DancingDots.Animation
  def init(opts) do
    {:ok, opts}
  end

  @impl DancingDots.Animation
  def handle_frame(dot, frame_number, _opts) when rem(frame_number, 4) != 0, do: dot

  def handle_frame(dot, _frame_number, _opts) do
    %{dot | opacity: dot.opacity / 2}
  end
end

defmodule DancingDots.Zoom do
  use DancingDots.Animation

  @impl DancingDots.Animation
  def init(opts) do
    case Keyword.get(opts, :velocity) do
      v when is_number(v) ->
        {:ok, opts}

      invalid ->
        {:error,
         "The :velocity option is required, and its value must be a number. Got: #{inspect(invalid)}"}
    end
  end

  @impl DancingDots.Animation
  def handle_frame(dot, frame_number, velocity: v) do
    %{dot | radius: dot.radius + (frame_number - 1) * v}
  end
end
