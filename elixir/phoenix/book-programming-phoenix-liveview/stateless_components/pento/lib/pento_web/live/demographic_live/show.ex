#---
# Excerpted from "Programming Phoenix LiveView",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit https://pragprog.com/titles/liveview for more book information.
#---
defmodule PentoWeb.DemographicLive.Show do
  use Phoenix.Component
  import Phoenix.HTML
  alias Pento.Survey.Demographic

  attr :demographic, Demographic, required: true

  def details(assigns) do
    ~H"""
    <div>
      <h2 class="font-medium text-2xl">
        Demographics {raw("&#x2713;")}
      </h2>
      <ul>
        <li>Gender: {@demographic.gender}</li>
        <li>
          Year of birth: {@demographic.year_of_birth}
        </li>
      </ul>
    </div>
    """
  end

end

