defmodule Lambda do
  alias :zip, as: Zip

  @name "greetingsOnDemand"

  def push() do
    client = %AWS.Client{
      access_key_id: System.get_env("ACCESS_KEY"),
      secret_access_key: System.get_env("ACCESS_SECRET"),
      region: "eu-central-1",
      endpoint: "amazonaws.com"
    }

    request =
      @name
      |> get_function_zip()
      |> build_request(@name)

    AWS.Lambda.create_function(client, request)
  end

  defp build_request(zipblob, name) do
    %{
      Code: %{
        ZipFile: zipblob
      },
      Environment: %{
        Variables: %{
          GREETING: "Hola"
        }
      },
      FunctionName: name,
      Handler: name <> ".handler",
      Runtime: "nodejs8.10",
      Version: "0.0.1",
      Role: "arn:aws:iam::095546587317:role/lambda_basic_execution"
    }
  end

  defp get_function_zip(name) do
    [ name ]
    |> zip(name <> ".zip")
    |> encode
  end

  defp zip(dirs, archive_name) when is_list(dirs) do
    dirs_list = dirs |> Enum.map(&String.to_char_list/1)
    Zip.create(String.to_charlist(archive_name), dirs_list, [:memory])
  end

  defp encode({:ok, {_, blob}}), do: Base.encode64(blob)
  defp encode({:error, error}), do: raise("Zipping went wrong: " <> Atom.to_string(error))
end
