defmodule Hub do

  HTTPotion.start

  "http://pokeapi.co/api/v2/pokemon/?limit=151&offset=0"
  |> HTTPotion.get([ "User-Agent": "Elixir" ])
  |> Map.get(:body)
  |> Poison.decode!
  |> Map.get("results")
  |> Enum.each(fn pokemon ->
    def unquote(String.to_atom(pokemon["name"]))() do
      unquote(Macro.escape(pokemon["url"]))
        |> HTTPotion.get([ "User-Agent": "Elixir" ])
        |> Map.get(:body)
        |> Poison.decode!
        |> Map.take(["id", "name", "base_experience", "stats", "types"])
    end
  end)
end
