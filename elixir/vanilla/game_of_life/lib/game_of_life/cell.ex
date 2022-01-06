defmodule GameOfLife.Cell do
  def keep_alive?(alive_cells, {x, y} = _alive_cell) do
    case count_neighbours(alive_cells, x, y) do
      2 -> true
      3 -> true
      _ -> false
    end
  end

  def dead_neighbours(alive_cells) do
    alive_cells
    |> neighbours()
    |> Enum.uniq()
    |> Kernel.--(alive_cells)
  end

  def become_alive?(alive_cells, {x, y} = _dead_cell),
    do: count_neighbours(alive_cells, x, y) == 3

  defp count_neighbours(alive_cells, x, y, count \\ 0)
  defp count_neighbours([], _x, _y, count), do: count
  defp count_neighbours([head | tail], x, y, count) do
    increment = case head do
      {hx, hy} when hx == x - 1 and hy == y - 1 -> 1 # ↙
      {hx, hy} when hx == x     and hy == y - 1 -> 1 # ↓
      {hx, hy} when hx == x + 1 and hy == y - 1 -> 1 # ↘

      {hx, hy} when hx == x - 1 and hy == y     -> 1 # ←
      {hx, hy} when hx == x + 1 and hy == y     -> 1 # →

      {hx, hy} when hx == x - 1 and hy == y + 1 -> 1 # ↖
      {hx, hy} when hx == x     and hy == y + 1 -> 1 # ↑
      {hx, hy} when hx == x + 1 and hy == y + 1 -> 1 # ↗

      _ -> 0
    end
    count_neighbours(tail, x, y, count + increment)
  end

  defp neighbours(alive_cells, neighbours \\ [])
  defp neighbours([], neighbours), do: neighbours
  defp neighbours([{x, y} | cells], neighbours) do
    neighbours(cells, neighbours ++ [
      {x - 1, y - 1}, {x    , y - 1}, {x + 1, y - 1},
      {x - 1, y    },                 {x + 1, y    },
      {x - 1, y + 1}, {x    , y + 1}, {x + 1, y + 1}
    ])
  end
end
