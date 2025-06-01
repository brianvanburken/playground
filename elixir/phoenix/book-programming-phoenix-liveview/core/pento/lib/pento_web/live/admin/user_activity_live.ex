#---
# Excerpted from "Programming Phoenix LiveView",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit https://pragprog.com/titles/liveview for more book information.
#---
defmodule PentoWeb.Admin.UserActivityLive do
  use PentoWeb, :live_component
  alias PentoWeb.Presence
  def update(_assigns, socket) do
    {:ok,
     socket
     |> assign_user_activity()}
  end


  def assign_user_activity(socket) do
    assign(socket, :user_activity, Presence.list_products_and_users())
  end

end
