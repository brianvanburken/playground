#---
# Excerpted from "Programming Phoenix LiveView",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit https://pragprog.com/titles/liveview for more book information.
#---
defmodule PentoWeb.SurveyLive do
  use PentoWeb, :live_view
  alias PentoWeb.SurveyLive.Component
  alias Pento.Survey
  alias PentoWeb.DemographicLive

  @impl true
  def mount(_params, _session, socket) do
    {:ok,
     socket
     |> assign_demographic}
  end


  defp assign_demographic(%{assigns: %{current_user: user}} = socket) do
    assign(
      socket,
      :demographic,
      Survey.get_demographic_by_user(user)
    )
  end


end

