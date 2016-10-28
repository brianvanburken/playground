defmodule PhoenixVideoStream.Util do
  def build_video_path(video) do
    Application.get_env(:phoenix_video_stream, :uploads_dir)
      |> Path.join(video.path)
  end
end
