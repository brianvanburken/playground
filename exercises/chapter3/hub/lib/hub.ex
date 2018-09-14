defmodule Hub do

  HTTPotion.start

  @doc """
  Fetch all the first 151 pokemons of the first generation and define methods
  for each based on the name. When called it uses the URL from the API result
  to fetch additional information on runtime. The fetching of additional
  information could be done on compile time. But to speed up the generation of
  the functions I decided to fetch it on runtime. It would take a long time to
  fetch 151 Pokemons using individual requests.
  """
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
