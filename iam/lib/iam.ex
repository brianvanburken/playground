defmodule IAM do
  def hello do
    %AWS.Client{
      access_key_id: System.get_env("ACCESS_KEY"),
      secret_access_key: System.get_env("ACCESS_SECRET"),
      region: "eu-central-1",
      endpoint: "amazonaws.com"
    }
    |> IO.inspect
    |> AWS.IAM.list_roles(%{})
    |> IO.inspect
  end
end
