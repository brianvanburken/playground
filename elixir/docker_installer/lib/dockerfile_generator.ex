defmodule DockerfileGenerator do
  def build do
    generate_dockerfile()
    # |> IO.inspect
    |> write_dockerfile("./greetingsOnDemand/")
    # |> IO.inspect
    |> build_project()
    # |> IO.inspect
    |> run_project()
    |> IO.inspect
    # |> fix_zip()
    # |> IO.inspect
    # |> store_zip()
    # |> IO.inspect
  end

  defp generate_dockerfile() do
    """
    FROM lambci/lambda:nodejs

    COPY . .

    """
    #    CMD zip lambda.zip . | base64 -w 0

  end

  defp write_dockerfile(dockerfile, path) do
    {File.write(path <> "Dockerfile", dockerfile), path}
  end

  defp build_project({:ok, path}) do
    {System.cmd("docker", ["build","-t","greetingsondemand","."], cd: path), path}
  end

  defp run_project({_, path}) do
    System.cmd("docker", ["run","--rm","greetingsondemand"], cd: path)
  end

  defp fix_zip({encoded, _}) do
    encoded
    |> String.replace("\n", "")
    |> String.trim()
    |> Base.decode64()
  end

  defp store_zip(encoded) do
    File.write("./greetings_on_demand.zip", encoded)
  end
end
