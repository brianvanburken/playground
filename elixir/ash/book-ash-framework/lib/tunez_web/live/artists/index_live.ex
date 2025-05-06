defmodule TunezWeb.Artists.IndexLive do
  use TunezWeb, :live_view

  require Logger

  def mount(_params, _session, socket) do
    socket =
      socket
      |> assign(:page_title, "Artists")

    {:ok, socket}
  end

  def handle_params(params, _url, socket) do
    sort_by = Map.get(params, "sort_by") |> validate_sort_by()
    query_text = Map.get(params, "q", "")
    page_params = AshPhoenix.LiveView.page_from_params(params, 12)

    page =
      Tunez.Music.search_artists!(
        query_text,
        page: page_params,
        query: [sort_input: sort_by],
        load: [:album_count, :latest_album_year_released, :cover_image_url]
      )

    socket =
      socket
      |> assign(:sort_by, sort_by)
      |> assign(:query_text, query_text)
      |> assign(:page, page)

    {:noreply, socket}
  end

  def render(assigns) do
    ~H"""
    <.header responsive={false}>
      <.h1>Artists</.h1>
      <:action>
        <.sort_changer selected={@sort_by} />
      </:action>
      <:action>
        <.search_box query={@query_text} method="get" phx-submit="search" data-role="artist-search" />
      </:action>
      <:action>
        <.button_link navigate={~p"/artists/new"} kind="primary">
          New Artist
        </.button_link>
      </:action>
    </.header>

    <div :if={@page.results == []} class="p-8 text-center">
      <.icon name="hero-face-frown" class="w-32 h-32 bg-gray-300" />
      <br /> No artist data to display!
    </div>

    <ul class="gap-6 lg:gap-12 grid grid-cols-2 sm:grid-cols-3 lg:grid-cols-4">
      <li :for={artist <- @page.results}>
        <.artist_card artist={artist} />
      </li>
    </ul>

    <.pagination_links page={@page} query={@query_text} sort_by={@sort_by} />
    """
  end

  def artist_card(assigns) do
    ~H"""
    <div id={"artist-#{@artist.id}"} data-role="artist-card" class="relative mb-2">
      <.link navigate={~p"/artists/#{@artist.id}"}>
        <.cover_image image={@artist.cover_image_url} />
      </.link>
    </div>
    <p>
      <.link
        navigate={~p"/artists/#{@artist.id}"}
        class="text-lg font-semibold"
        data-role="artist-name"
      >
        {@artist.name}
      </.link>
    </p>

    <.artist_card_album_info artist={@artist} />
    """
  end

  def artist_card_album_info(%{artist: %{album_count: 0}} = assigns), do: ~H""

  def artist_card_album_info(assigns) do
    ~H"""
    <span class="mt-2 text-sm leading-6 text-zinc-500">
      {@artist.album_count} {ngettext("album", "albums", @artist.album_count)},
      latest release {@artist.latest_album_year_released}
    </span>
    """
  end

  def pagination_links(assigns) do
    ~H"""
    <div
      :if={AshPhoenix.LiveView.prev_page?(@page) || AshPhoenix.LiveView.next_page?(@page)}
      class="flex justify-center pt-8 space-x-4"
    >
      <.button_link
        data-role="previous-page"
        kind="primary"
        inverse
        patch={~p"/?#{query_string(@page, @query, @sort_by, "prev")}"}
        disabled={!AshPhoenix.LiveView.prev_page?(@page)}
      >
        « Previous
      </.button_link>
      <.button_link
        data-role="next-page"
        kind="primary"
        inverse
        patch={~p"/?#{query_string(@page, @query, @sort_by, "next")}"}
        disabled={!AshPhoenix.LiveView.next_page?(@page)}
      >
        Next »
      </.button_link>
    </div>
    """
  end

  attr(:query, :string, default: "")
  attr(:rest, :global, include: ~w(method action phx-submit data-role))
  slot(:inner_block, required: false)

  def search_box(assigns) do
    ~H"""
    <form class="relative w-fit inline-block" {@rest}>
      <.icon name="hero-magnifying-glass" class="w-4 h-4 m-2 ml-3 mt-4 absolute bg-gray-400" />
      <label for="search-text" class="hidden">Search</label>
      <.input
        class="!rounded-full p-1 pl-8 !w-32 sm:!w-48"
        name="query"
        id="search-text"
        value={@query}
      />
      {render_slot(@inner_block)}
    </form>
    """
  end

  def sort_changer(assigns) do
    assigns = assign(assigns, :options, sort_options())

    ~H"""
    <form data-role="artist-sort" class="hidden sm:inline" phx-change="change-sort">
      <.input
        label="sort by:"
        type="select"
        id="sort_by"
        name="sort_by"
        options={@options}
        value={@selected}
        class="px-2 py-0.5 !w-fit !inline-block pr-8 text-sm"
        container_class="!inline-block"
      />
    </form>
    """
  end

  defp sort_options do
    [
      {"recently updated", "-updated_at"},
      {"recently added", "-inserted_at"},
      {"name", "name"},
      {"number of albums", "-album_count"},
      {"latest album release", "--latest_album_year_released"}
    ]
  end

  def validate_sort_by(key) do
    valid_keys = Enum.map(sort_options(), &elem(&1, 1))

    if key in valid_keys do
      key
    else
      List.first(valid_keys)
    end
  end

  def query_string(page, query_text, sort_by, which) do
    case AshPhoenix.LiveView.page_link_params(page, which) do
      :invalid -> []
      list -> list
    end
    |> Keyword.put(:q, query_text)
    |> Keyword.put(:sort_by, sort_by)
    |> remove_empty()
  end

  defp remove_empty(params) do
    Enum.filter(params, fn {_key, val} -> val != "" end)
  end

  def handle_event("change-sort", %{"sort_by" => sort_by}, socket) do
    params = remove_empty(%{q: socket.assigns.query_text, sort_by: sort_by})
    {:noreply, push_patch(socket, to: ~p"/?#{params}")}
  end

  def handle_event("search", %{"query" => query}, socket) do
    params = remove_empty(%{q: query, sort_by: socket.assigns.sort_by})
    {:noreply, push_patch(socket, to: ~p"/?#{params}")}
  end
end
