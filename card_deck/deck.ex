# Source: https://github.com/knewter/elixir_card_deck

defmodule ElixirCardDeck do
  def make_deck do
    cards = [:a, 2, 3, 4, 5, 6, 7, 8, 9, 10, :j, :q, :k]
    types = [:spades, :clubs, :diamonds, :hearts]
    for x <- cards, y <- types, do: {:card, x, y}
  end
end
