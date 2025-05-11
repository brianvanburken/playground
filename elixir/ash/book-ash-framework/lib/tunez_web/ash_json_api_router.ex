defmodule TunezWeb.AshJsonApiRouter do
  use AshJsonApi.Router,
    domains: [Tunez.Music, Tunez.Accounts],
    open_api: "/open_api"
end
