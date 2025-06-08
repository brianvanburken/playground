defmodule PentoWeb.ProductLive.Show do
  use PentoWeb, :live_view

  alias Pento.Catalog

  @impl true
  def mount(_params, _session, socket) do
    {:ok, socket}
  end

  @impl true
  def handle_params(%{"id" => id}, _, socket) do
    {:noreply,
     socket
     |> assign(:page_title, page_title(socket.assigns.live_action))
     |> assign(:product, Catalog.get_product!(id))}
  end

  defp page_title(:show), do: "Show Product"
  defp page_title(:edit), do: "Edit Product"
end

# Questions chapter 3
# - Which route gets invoked when you click the link on the Index page to view a given product?
#   A: this file
# - What data does ProductLive.Show.mount/3 add to the socket?
#   A: Nothing
# - How does the ProductLive.Show live view use the handle_params/3 callback?
#   A: to set the product using the ID from the params and the title based on the action
# - How does the ProductLive.Show template render the Product Edit form and what events does that form support?
#   A: It renders the details of a product and a button on top to edit through a modal containing a live form
