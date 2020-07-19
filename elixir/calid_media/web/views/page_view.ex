defmodule CalidMedia.PageView do
  use CalidMedia.Web, :view

  def filetype(filename) do
    filename
    |> String.split(".")
    |> Enum.at(1)
    |> get_file_type
  end

  defp get_file_type("mp3"), do: "mpeg"
  defp get_file_type("mp4"), do: "mp4"
end
