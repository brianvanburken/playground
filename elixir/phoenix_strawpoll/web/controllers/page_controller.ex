defmodule PhoenixStrawpoll.PageController do
  use PhoenixStrawpoll.Web, :controller

  def index(conn, _params) do
    render conn, "index.html"
  end
end
