defmodule GithubInstallTest do
  use ExUnit.Case
  doctest GithubInstall

  test "greets the world" do
    assert GithubInstall.hello() == :world
  end
end
