defmodule Encoder do

  def execute() do
    "test.zip"
    |> File.read!
    |> Base.encode64()
    |> prepend_data_url()
  end

  defp prepend_data_url(data), do: "data:application/zip;base64," <> data
end
