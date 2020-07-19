defmodule IAM do
  def hello do
    HTTPoison.start()

    url()
    |> IO.inspect()
    |> HTTPoison.get!()
  end

  defp url() do
    params = get_parameters()

    "https://iam.amazonaws.com/"
    <> "?" <> params
    <> "&Signature=" <> get_signature(params)
  end

  defp get_parameters() do
    [
      "Action=ListRoles",
      "Version=2010-05-08",
      "MaxItems=10",
      # Authentication
      "SignatureVersion=2",
      "SignatureMethod=HmacSHA256",
      "AWSAccessKeyId=" <> System.get_env("ACCESS_KEY"),
      "Timestamp=" <> get_timestamp()
    ]
    |> Enum.sort()
    |> Enum.join("&")
    |> IO.inspect
  end

  defp get_timestamp() do
    DateTime.utc_now()
    |> DateTime.to_iso8601()
    |> String.split(".")
    |> Kernel.hd()
    |> URI.encode_www_form()
  end

  # https://docs.aws.amazon.com/general/latest/gr/signature-version-2.html
  defp get_signature(params) do
    signature = "GET\n"
    <> "iam.amazonaws.com\n"
    <> "/\n"
    <> params

    :crypto.hmac(:sha256, System.get_env("ACCESS_SECRET"), signature)
    |> Base.encode64(padding: false)
    |> IO.inspect
  end
end
