[
  import_deps: [
    :oban,
    :ash_oban,
    :ash_authentication_phoenix,
    :ash_authentication,
    :ash_graphql,
    :absinthe,
    :ash_json_api,
    :ash_postgres,
    :ash,
    :reactor,
    :ecto,
    :ecto_sql,
    :phoenix
  ],
  subdirectories: ["priv/*/migrations"],
  plugins: [Absinthe.Formatter, Spark.Formatter, Phoenix.LiveView.HTMLFormatter],
  inputs: ["*.{heex,ex,exs}", "{config,lib,test}/**/*.{heex,ex,exs}", "priv/*/seeds.exs"]
]
