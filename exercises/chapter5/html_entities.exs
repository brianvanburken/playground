defmodule HtmlEntities do
  @external_resource html_entities_path = Path.join([__DIR__, "entities.txt"])

  @doc """
  Define the methods for replacing individual chars that are listed in the
  entities file. This wil return their encoded equivalent.
  """
  for line <- File.stream!(html_entities_path, [], :line) do
    [encoded, decoded, _] = line |> String.strip |> String.split(",")
    defp replace_char(unquote(decoded)), do: "&#{unquote(encoded)};"
  end

  @doc """
  If there is no entity in the list that matches the one given it just gets
  returned.
  """
  defp replace_char(ch), do: ch

  @doc """
  This takes a string and splits it on the graphemes to get a list of all the
  characters in the string. Then each individual character is replace with their
  equivalent encoded entity if present. Then the string is joined back together
  and returned.
  """
  def encode(string) do
    string
    |> String.graphemes
    |> Enum.map(&replace_char/1)
    |> Enum.join
  end
end
