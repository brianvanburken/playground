defmodule CalidMedia.PageController do
  use CalidMedia.Web, :controller

  def index(conn, _params) do
    render conn, "index.html", files: media_files
  end

  def show(conn, %{"filename" => filename}) do
    render conn, "show.html", filename: filename
  end

  defp media_files do
    media_dir = "./priv/static/media"
    filetype = [".mp4", ".mp3"]
    {:ok, files} = File.ls(media_dir)
    files
    |> Stream.filter(fn(file) -> String.ends_with?(file, filetype) end)
    |> Stream.map(fn(file) -> %{filename: file} end)
    |> Enum.to_list
  end
end
