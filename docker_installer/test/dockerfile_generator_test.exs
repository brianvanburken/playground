defmodule DockerfileGeneratorTest do
  use ExUnit.Case
  doctest DockerfileGenerator

  test "greets the world" do
    assert DockerfileGenerator.hello() == :world
  end
end
