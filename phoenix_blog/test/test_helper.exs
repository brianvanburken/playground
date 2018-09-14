ExUnit.start

Mix.Task.run "ecto.create", ~w(-r BlogPhoenix.Repo --quiet)
Mix.Task.run "ecto.migrate", ~w(-r BlogPhoenix.Repo --quiet)
Ecto.Adapters.SQL.begin_test_transaction(BlogPhoenix.Repo)

