defmodule Helloplug do
  def init(default_opts) do
    IO.puts "starting up Helloplug..."
    default_opts
  end

  def call(conn, _opts) do
    IO.puts "saying Hello!"
    conn |> route
  end

  def route(%{method: "GET", path_info: ["hello"]} = conn) do
    conn |> Plug.Conn.send_resp(200, "Hello, world!")
  end

  def route(%{method: "GET", path_info: ["users", user_id]} = conn) do
    conn |> Plug.Conn.send_resp(200, "You requested user #{user_id}")
  end

  def route(conn),
    do: conn |> Plug.Conn.send_resp(404, "Couldn't find that page, sorry!")
end
