defmodule Helloplug do
  def init(default_opts) do
    IO.puts "starting up Helloplug..."
    default_opts
  end

  def call(conn, _opts) do
    IO.puts "saying Hello!"
    route(conn.method, conn.path_info, conn)
  end

  def route("GET", ["hello"], conn) do
    conn |> Plug.Conn.send_resp(200, "Hello, world!")
  end

  def route("GET", ["users", user_id], conn) do
    conn |> Plug.Conn.send_resp(200, "You requested user #{user_id}")
  end

  def route(_method, _path, conn),
    do: conn |> Plug.Conn.send_resp(404, "Couldn't find that page, sorry!")
end
