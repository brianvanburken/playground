defmodule Rumbl.Counter do
  use GenServer

  def inc(pid), do: GenServer.cast(pid, :inc)

  def dec(pid), do: GenServer.cast(pid, :dec)

  def val(pid), do: GenServer.call(pid, :val)

  def start_link(val), do: GenServer.start_link(__MODULE__, val)

  def init(val) do
    Process.send_after(self, :tick, 1000)
    {:ok, val}
  end

  def handle_cast(:inc, val), do: {:noreply, val + 1}

  def handle_cast(:dec, val), do: {:noreply, val - 1}

  def handle_call(:val, _from, val), do: {:reply, val, val}

  def handle_info(:tick, val) when val <= 0, do: raise "Boom!"
  def handle_info(:tick, val) do
    IO.puts "tick #{val}"
    Process.send_after(self, :tick, 1000)
    {:noreply, val - 1}
  end
end
