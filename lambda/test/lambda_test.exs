defmodule LambdaTest do
  use ExUnit.Case
  doctest Lambda

  test "greets the world" do
    assert Lambda.hello() == :world
  end
end
