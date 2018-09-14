#---
# Excerpted from "Programming Phoenix",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/phoenix for more book information.
#---
use Mix.Config

# In this file, we keep production configuration that
# you likely want to automate and keep it away from
# your version control system.
config :hello, Hello.Endpoint,
  secret_key_base: "VhnAtthaew3PT+JQILGUDUaw6tCfP27hBv68aN2Kl/OjbPEUZALPN7qaipgsCKdx"

# Configure your database
config :hello, Hello.Repo,
  adapter: Ecto.Adapters.Postgres,
  username: "postgres",
  password: "postgres",
  database: "hello_prod",
  pool_size: 20
