#---
# Excerpted from "Programming Phoenix LiveView",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit https://pragprog.com/titles/liveview for more book information.
#---
defmodule Pento.Game.Board do
  alias Pento.Game.{Pentomino, Shape}

  defstruct active_pento: nil,
            completed_pentos: [],
            palette: [],
            points: []

  def puzzles(), do: ~w[tiny small ball donut default wide widest medium]a

  def new(palette, points, hole \\ []) do
    %__MODULE__{palette: palette(palette), points: points -- hole}
  end


  def new(:tiny), do: new(:small, rect(5, 3))
  def new(:widest), do: new(:all, rect(20, 3))
  def new(:wide), do: new(:all, rect(15, 4))
  def new(:medium), do: new(:all, rect(12, 5))
  def new(:default), do: new(:all, rect(10, 6))
  def new(:small), do: new(:medium, rect(7, 5))

  def new(:donut) do
    new(:all, rect(8, 8), for(x <- 4..5, y <- 4..5, do: {x, y}))
  end

  def new(:ball) do
    new(:all, rect(8, 8), for(x <- [1, 8], y <- [1, 8], do: {x, y}))
  end


  def to_shape(board) do
    Shape.__struct__(color: :purple, name: :board, points: board.points)
  end


  def to_shapes(board) do
    board_shape = to_shape(board)

    pento_shapes =
      [board.active_pento | board.completed_pentos]
      |> Enum.reverse()
      |> Enum.filter(& &1)
      |> Enum.map(&Pentomino.to_shape/1)

    [board_shape | pento_shapes]
  end


  def active?(board, shape_name) when is_binary(shape_name) do
    # ensure atoms exist
    puzzles()
    active?(board, String.to_existing_atom(shape_name))
  end

  def active?(%{active_pento: %{name: shape_name}}, shape_name), do: true
  def active?(_board, _shape_name), do: false

  defp rect(x, y) do
    for x <- 1..x, y <- 1..y, do: {x, y}
  end

  defp palette(:all), do: [:i, :l, :y, :n, :p, :w, :u, :v, :s, :f, :x, :t]
  defp palette(:small), do: [:u, :v, :p]
  defp palette(:medium), do: [:t, :y, :l, :p, :n, :v, :u]

  def pick(board, :board), do: board
  def pick(%{active_pento: pento} = board, sname) when not is_nil(pento) do
    if pento.name == sname do
      %{board | active_pento: nil}
    else
      board
    end
  end

  def pick(board, shape_name) do
    active =
      board.completed_pentos
      |> Enum.find(&(&1.name == shape_name))
      |> Kernel.||(new_pento(board, shape_name))

    completed = Enum.filter(board.completed_pentos, &(&1.name != shape_name))

    %{board | active_pento: active, completed_pentos: completed}
  end


  defp new_pento(board, shape_name) do
    Pentomino.new(name: shape_name, location: midpoints(board))
  end

  defp midpoints(board) do
    {xs, ys} = Enum.unzip(board.points)
    {midpoint(xs), midpoint(ys)}
  end

  defp midpoint(i), do: round(Enum.max(i) / 2.0)

  def drop(%{active_pento: nil} = board), do: board

  def drop(%{active_pento: pento} = board) do
    board
    |> Map.put(:active_pento, nil)
    |> Map.put(:completed_pentos, [pento | board.completed_pentos])
  end


  def legal_move?(%{active_pento: pento, points: points} = _board) do
    pento.location in points
  end


  def legal_drop?(%{active_pento: pento}) when is_nil(pento), do: false

  def legal_drop?(%{active_pento: pento, points: board_points} = board) do
    points_on_board =
      Pentomino.to_shape(pento).points
      |> Enum.all?(fn point -> point in board_points end)

    no_overlapping_pentos =
      !Enum.any?(board.completed_pentos, &Pentomino.overlapping?(pento, &1))

    points_on_board and no_overlapping_pentos
  end

end
