ExUnit.start

Mix.Task.run "ecto.create", ~w(-r VideoChat.Repo --quiet)
Mix.Task.run "ecto.migrate", ~w(-r VideoChat.Repo --quiet)
Ecto.Adapters.SQL.begin_test_transaction(VideoChat.Repo)

