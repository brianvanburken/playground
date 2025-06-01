defmodule Pento.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  @impl true
  def start(_type, _args) do
    children = [
      PentoWeb.Telemetry,
      Pento.Repo,
      {DNSCluster, query: Application.get_env(:pento, :dns_cluster_query) || :ignore},
      {Phoenix.PubSub, name: Pento.PubSub},
      # Start the Finch HTTP client for sending emails
      {Finch, name: Pento.Finch},
      # Start a worker by calling: Pento.Worker.start_link(arg)
      # {Pento.Worker, arg},
      # Start to serve requests, typically the last entry
      PentoWeb.Endpoint
    ]

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: Pento.Supervisor]
    Supervisor.start_link(children, opts)
  end

  # Tell Phoenix to update the endpoint configuration
  # whenever the application is updated.
  @impl true
  def config_change(changed, _new, removed) do
    PentoWeb.Endpoint.config_change(changed, removed)
    :ok
  end
end
