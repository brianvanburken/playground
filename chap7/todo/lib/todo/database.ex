defmodule Todo.Database do
  use GenServer

  @pool_size 3

  def start(db_folder) do
    GenServer.start(__MODULE__, db_folder, name: :database_server)
  end

  def store(key, data) do
    key
    |> choose_worker
    |> Todo.DatabaseWorker.store(key, data)
  end

  def get(key) do
    key
    |> choose_worker
    |> Todo.DatabaseWorker.get(key)
  end

  # Choosing a worker makes a request to the :database_server process. There we
  # keep the knowledge about our workers, and return the pid of the corresponding
  # worker. Once this is done, the caller process will talk to the worker directly.
  defp choose_worker(key) do
    GenServer.call(:database_server, {:choose_worker, key})
  end

  def init(db_folder) do
    {:ok, start_workers(db_folder)}
  end

  # This function creates a pool of database workers and stores their PID in a
  # Map.
  defp start_workers(db_folder) do
    for index <- 1..@pool_size,
        {:ok, pid} = Todo.DatabaseWorker.start(db_folder),
        into: %{},
        do: {index - 1, pid}
  end

  # This function receives the synchronous call to pick a worker from the pool
  # and returns it. It uses the :erlang.phash2/2 to retrieve a key within the
  # limits of the pool. phash2/2 is a function that returns always the same hash
  # between 0 and the range given. In this instance the max of the range is the
  # length of the poolsize. That way if you give for example "bobs_list" for a
  # name it will always return the key 1 with a poolsize of 3. Thus the "bobs_list"
  # is always handled by the worker that is stored behind index 1 of the pool.
  def handle_call({:choose_worker, key}, _, workers) do
    worker_key = :erlang.phash2(key, @pool_size)
    {:reply, Map.get(workers, worker_key), workers}
  end
end
