defmodule LibraryFees do
  def datetime_from_string(string) do
    string
    |> NaiveDateTime.from_iso8601!()
  end

  def before_noon?(datetime) do
    datetime
    |> NaiveDateTime.to_time()
    |> Time.compare(~T[12:00:00])
    |> (fn res -> res == :lt end).()
  end

  def return_date(checkout_datetime) do
    days =
      if before_noon?(checkout_datetime) do
        28
      else
        29
      end

    seconds = days * 24 * 60 * 60

    checkout_datetime
    |> NaiveDateTime.add(seconds)
    |> NaiveDateTime.to_date()
  end

  def days_late(planned_return_date, actual_return_datetime) do
    actual_return_date = NaiveDateTime.to_date(actual_return_datetime)

    planned_return_date
    |> Date.compare(actual_return_date)
    |> case do
      :lt -> Date.diff(actual_return_date, planned_return_date)
      _ -> 0
    end
  end

  def monday?(datetime) do
    datetime
    |> Date.day_of_week()
    |> (fn d -> d == 1 end).()
  end

  def calculate_late_fee(checkout, return, rate) do
    return_date =
      return
      |> datetime_from_string()

    is_monday = monday?(return_date)

    checkout
    |> datetime_from_string()
    |> return_date()
    |> days_late(return_date)
    |> case do
      days when is_monday -> trunc(days * rate * 0.5)
      days -> days * rate
    end
  end
end
