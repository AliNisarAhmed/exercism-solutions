defmodule BankAccount do
  @moduledoc """
  A bank account that supports access from multiple processes.
  """

  @typedoc """
  An account handle.
  """
  @opaque account :: pid

  @doc """
  Open the bank. Makes the account available.
  """
  @spec open_bank() :: account
  def open_bank() do
    with {:ok, pid} <- Agent.start_link(fn -> {:open, 0} end) do
      pid
    end
  end

  @doc """
  Close the bank. Makes the account unavailable.
  """
  @spec close_bank(account) :: none
  def close_bank(account) do
    Agent.update(account, fn {_status, balance} -> {:closed, balance} end)
  end

  @doc """
  Get the account's balance.
  """
  @spec balance(account) :: integer
  def balance(account) do
    Agent.get(account, fn
      {:open, balance} -> balance
      {:closed, _balance} -> {:error, :account_closed}
    end)
  end

  @doc """
  Update the account's balance by adding the given amount which may be negative.
  """
  @spec update(account, integer) :: any
  def update(account, amount) do
    Agent.get_and_update(account, fn
      {:closed, _balance} = state -> {{:error, :account_closed}, state}
      {:open, balance} -> {balance + amount, {:open, balance + amount}}
    end)
  end
end
