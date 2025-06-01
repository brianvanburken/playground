#---
# Excerpted from "Programming Phoenix LiveView",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit https://pragprog.com/titles/liveview for more book information.
#---
defmodule PentoWeb.RatingLive.Index do
  use Phoenix.Component
  alias PentoWeb.RatingLive
  alias PentoWeb.RatingLive.Show

  defp ratings_complete?(products) do
    Enum.all?(products, fn product ->
      not Enum.empty?(product.ratings)
    end)
  end


  attr(:products, :list, required: true)
  attr(:current_user, :any, required: true)

  def product_list(assigns) do
    ~H"""
    <.heading products={@products} />
    <div class="grid grid-cols-2 gap-4 divide-y">
      <.product_rating
        :for={{p, i} <- Enum.with_index(@products)}
        current_user={@current_user}
        product={p}
        index={i}
      />
    </div>
    """
  end


  attr(:product, :any, required: true)
  attr(:current_user, :any, required: true)
  attr(:index, :integer, required: true)

  def product_rating(assigns) do
    ~H"""
    <div>{@product.name}</div>
    <%= if rating = List.first(@product.ratings) do %>
      <Show.stars rating={rating} />
    <% else %>
      <div>
        <.live_component
          module={RatingLive.Form}
          id={"rating-form-#{@product.id}"}
          product={@product}
          product_index={@index}
          current_user={@current_user}
        />
      </div>
    <% end %>
    """
  end


  attr(:products, :list, required: true)

  def heading(assigns) do
    ~H"""
    <h2 class="font-medium text-2xl">
      Ratings {if ratings_complete?(@products),
        do: Phoenix.HTML.raw("&#x2713;")}
    </h2>
    """
  end

end
