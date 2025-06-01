#---
# Excerpted from "Programming Phoenix LiveView",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit https://pragprog.com/titles/liveview for more book information.
#---
defmodule PentoWeb.GameLive.Picker do
  use PentoWeb, :live_view

  alias Pento.Game.Board
  import PentoWeb.GameLive.{Colors, Component}

  def mount(_params, _session, socket) do
    {:ok, assign_boards(socket)}
  end

  def assign_boards(socket) do
    assign(
      socket,
      :boards,
      Board.puzzles()
      |> Enum.map(&{&1, Board.new(&1)})
    )
  end


  def render(assigns) do
    ~H"""
    <h1 class="font-heavy text-4xl text-center mb-6">
      Choose a Puzzle
    </h1>
    <%= for {puzzle, board} <- @boards do %>
      <.row board={board} puzzle={puzzle} />
    <% end %>
    """
  end


  attr :board, :any, required: true
  attr :puzzle, :atom, required: true

  def row(assigns) do
    ~H"""
    <.link navigate={~p"/game/#{@puzzle}"}>
      <div class="grid grid-cols-2 hover:bg-slate-200">
        <div>
          <h3 class="text-2xl">Pieces</h3>
        </div>
        <div>
          <h3 class="text-2xl">
            {@puzzle |> to_string |> String.capitalize()} Puzzle
          </h3>
        </div>
        <.palette shape_names={@board.palette} />
        <.board board={@board} />
      </div>
    </.link>
    """
  end


  attr :board, :any, required: true

  def board(assigns) do
    ~H"""
    <div>
      <.canvas view_box={"0 0 400 #{height(@board) * 10 + 25}"}>
        <.shape
          points={Board.to_shape(@board).points}
          fill={color(:purple)}
          name="board"
        />
      </.canvas>
    </div>
    """
  end


  defp height(board) do
    board.points
    |> Enum.map(fn {_, y} -> y end)
    |> Enum.max()
  end

end
