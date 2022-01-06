defmodule Zips do

  alias :zip, as: Zip

  defp zip(archive_name, dirs) when is_list(dirs) do
    dirs_list = dirs
                |> Enum.map(&String.to_charlist/1)
    Zip.create(String.to_charlist(archive_name), dirs_list)
  end

  def execute, do: zip("test.zip", [ "./example" ])
end
