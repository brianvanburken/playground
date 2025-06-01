#---
# Excerpted from "Programming Phoenix LiveView",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit https://pragprog.com/titles/liveview for more book information.
#---
defmodule PentoWeb.GameLive do
  use PentoWeb, :live_view


  alias PentoWeb.GameLive.Board

  def mount(%{"puzzle" => puzzle}, _session, socket) do
    {:ok, assign(socket, puzzle: puzzle)}
  end


  def render(assigns) do
    ~H"""
    <section class="container">
      <div class="grid grid-cols-2">
        <div>
          <h1 class="font-heavy text-3xl text-center">
            Welcome to Pento!
          </h1>
        </div>
        <.help />
      </div>
      <.live_component module={Board} puzzle={@puzzle} id="game" />
    </section>
    """
  end


  def help(assigns) do
    ~H"""
    <div>
      <.help_button />
      <.help_page />
    </div>
    """
  end


  attr :class, :string, default: "h-8 w-8 text-slate hover:text-slate-400"

  def help_button(assigns) do
    ~H"""
    <button phx-click={JS.toggle(to: "#info")}>
      <.icon name="hero-question-mark-circle-solid" class={@class} />
    </button>
    """
  end


  def help_page(assigns) do
    ~H"""
    <div id="info" hidden class="fixed hidden bg-white mx-4 border border-2">
      <ul class="mx-8 list-disc">
        <li>Click on a pento to pick it up</li>
        <li>Drop a pento with a space</li>
        <li>Pentos can't overlap</li>
        <li>Pentos must be fully on the board</li>
        <li>Rotate a pento with shift</li>
        <li>Flip a pento with enter</li>
        <li>Place all the pentos to win</li>
      </ul>
    </div>
    """
  end

end
