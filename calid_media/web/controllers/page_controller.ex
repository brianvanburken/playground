defmodule CalidMedia.PageController do
  use CalidMedia.Web, :controller

  def index(conn, _params) do
    media_dir = "./priv/static/media"
    filetype = [".mp4", ".mp3"]
    {:ok, files} = File.ls(media_dir)
    filtered_files = Enum.filter(files, fn(file) -> String.ends_with?(file, filetype) end)
    render conn, "index.html", files: filtered_files
  end
end
