Code.require_file("assertion.exs", __DIR__)
Code.require_file("loop.exs", __DIR__)

defmodule LoopTest do
  use Assertion
  import Loop

  test "A dummy test" do
    assert Code.ensure_loaded?(Loop)
  end

  test "while/2 loops as long as the expression is truthy" do
    pid = spawn(fn -> :timer.sleep(:infinity) end)

    send self, :one
    while Process.alive?(pid) do
      receive do
        :one -> send self, :two
        :two -> send self, :three
        :three ->
          Process.exit(pid, :kill)
          send self, :done
      end
    end
    assert Process.info(self)[:messages] == [:done]
  end

end

LoopTest.run
