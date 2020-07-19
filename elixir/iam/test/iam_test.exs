defmodule IamTest do
  use ExUnit.Case
  doctest Iam

  test "greets the world" do
    assert Iam.hello() == :world
  end
end
