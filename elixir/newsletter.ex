defmodule Newsletter do
  def read_emails(path) do
    path
    |> File.read!()
    |> String.split()
  end

  def open_log(path) do
    path
    |> File.open!([:read, :write])
  end

  def log_sent_email(pid, email) do
    pid
    |> IO.binwrite(email <> "\n")
  end

  def close_log(pid) do
    pid
    |> File.close()
  end

  def send_newsletter(emails_path, log_path, send_fun) do
    emails = read_emails(emails_path)

    log = open_log(log_path)

    for email <- emails do
      case send_fun.(email) do
        :ok -> log_sent_email(log, email)
        _ -> nil
      end
    end

    close_log(log)
  end
end
