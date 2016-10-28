defmodule PhoenixVideoStream.Util do
  def build_video_path(video) do
    Application.get_env(:phoenix_video_stream, :uploads_dir)
      |> Path.join(video.path)
  end

  def send_video(conn, headers, video) do
    video_path = build_video_path(video)
    conn
    |> Plug.Conn.put_resp_header("content-type", video.content_type)
    |> Plug.Conn.send_file(200, video_path)
  end
end
