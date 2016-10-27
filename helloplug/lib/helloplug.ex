defmodule Router do
  defmacro __using__(opts) do
    quote do
      def init(options), do: options

      def call(conn, _opts), do: route(conn.method, conn.path_info, conn)
    end
  end
end

defmodule Helloplug do
  use Router

  def route("GET", ["hello"], conn) do
    conn |> Plug.Conn.send_resp(200, "Hello, world!")
  end

  def route("GET", ["users", user_id], conn) do
    conn |> Plug.Conn.send_resp(200, "You requested user #{user_id}")
  end

  def route(_method, _path, conn),
    do: conn |> Plug.Conn.send_resp(404, "Couldn't find that page, sorry!")
end
