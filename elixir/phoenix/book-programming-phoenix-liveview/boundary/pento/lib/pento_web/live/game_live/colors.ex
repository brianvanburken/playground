#---
# Excerpted from "Programming Phoenix LiveView",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit https://pragprog.com/titles/liveview for more book information.
#---
defmodule PentoWeb.GameLive.Colors do
  def color(c), do: color(c, false, false)

  def color(_color, true, _completed), do: "#B86EF0"
  def color(_color, _active, true), do: "#000000"
  def color(:green, _active, _completed), do: "#8BBF57"
  def color(:dark_green, _active, _completed), do: "#689042"
  def color(:light_green, _active, _completed), do: "#C1D6AC"
  def color(:orange, _active, _completed), do: "#B97328"
  def color(:dark_orange, _active, _completed), do: "#8D571E"
  def color(:light_orange, _active, _completed), do: "#F4CCA1"
  def color(:gray, _active, _completed), do: "#848386"
  def color(:dark_gray, _active, _completed), do: "#5A595A"
  def color(:light_gray, _active, _completed), do: "#B1B1B1"
  def color(:blue, _active, _completed), do: "#83C7CE"
  def color(:dark_blue, _active, _completed), do: "#63969B"
  def color(:light_blue, _active, _completed), do: "#B9D7DA"
  def color(:purple, _active, _completed), do: "#240054"
end
