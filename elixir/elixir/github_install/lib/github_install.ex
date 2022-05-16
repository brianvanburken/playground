defmodule GithubInstall do
  alias :zip, as: Zip

  def install do
    HTTPoison.start()

    group = "<group>"
    repo = "<repo>"

    headers = [ "User-Agent": "Mozilla/5.0 (Windows NT 5.1; rv:23.0) Gecko/20100101 Firefox/23.0" ]
    url = "https://codeload.github.com/#{group}/#{repo}/zip/master"

    url
    |> HTTPoison.get!(headers)
    |> write
    |> unzip
    |> npm_install(repo)
  end

  defp write(%HTTPoison.Response{body: body}) do
    File.write!("test.zip", body)
  end

  defp unzip(:ok) do
    Zip.unzip('test.zip')
  end

  defp npm_install(_, repo) do
    System.cmd("npm", ["install"], cd: repo <> "-master")
  end
end
