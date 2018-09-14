defmodule RoundRobin do
  require Integer

  def schedule(teams) do
    teams
    |> round_out
    |> build_schedule
  end

  defp build_schedule(teams) do
    l = length(teams)
    _build_schedule(Enum.chunk(teams, (l/2 |> float_to_int)), [], number_of_rounds(l))
  end

  defp _build_schedule(_, output, 0), do: output
  defp _build_schedule([t1, t2], output, l) do
    output = [t1, t2]
    |> generate_matches
    |> combine_with(output)

    rotate(t1, t2)
    |> _build_schedule(output, l - 1)
  end

  defp rotate([h1|htail], [a1|atail]) do
    [x|xt] = Enum.reverse(htail)
    [
      [h1, a1|Enum.reverse(xt)],
      atail ++ [x]
    ]
  end

  defp combine_with(round, output), do: output ++ [round]

  defp generate_matches([t1, t2]), do: Enum.zip(t1, t2)

  defp number_of_rounds(number_of_teams), do: (number_of_teams - 1) * 2

  defp round_out(teams) when rem(length(teams), 2) === 0, do: teams
  defp round_out(teams), do: teams ++ [:bye]

  defp float_to_int(f) do
    {n, _} = f
      |> to_string
      |> Integer.parse
    n
  end
end
