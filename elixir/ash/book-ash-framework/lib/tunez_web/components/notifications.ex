defmodule TunezWeb.Notifications do
  use TunezWeb, :live_component

  def render(assigns) do
    ~H"""
    <div class="dropdown dropdown-end">
      <div tabindex="0" class="p-1 mt-1 cursor-pointer relative">
        <.icon name="hero-bell-alert" class="w-8 h-8 bg-base-content/40" />
        <span :if={@notifications != []} class="absolute flex h-3 w-3 top-0 right-0 mt-1 mr-1.5">
          <span class="animate-ping absolute inline-flex h-full w-full rounded-full bg-error opacity-75">
          </span>
          <span class="relative inline-flex rounded-full h-3 w-3 bg-error"></span>
        </span>
      </div>
      <div
        :if={@notifications == []}
        class="dropdown-content z-[1] p-2 shadow bg-base-100 rounded-box w-52"
      >
        <.icon name="hero-check-circle" class="w-8 h-8 bg-success/50" />
        <span class="text-sm px-2">No new notifications!</span>
      </div>
      <ul
        :if={@notifications != []}
        tabindex="0"
        class="menu menu-sm dropdown-content z-[1] p-2 shadow bg-base-100 rounded-box w-80"
      >
        <li
          :for={notification <- @notifications}
          phx-click="dismiss-notification"
          phx-value-id={notification.id}
          phx-target={@myself}
        >
          <div>
            <p>
              The album <span class="font-bold">{notification.album.name}</span>
              has been added for {notification.album.artist.name}
            </p>
            <div class="h-16 w-16">
              <.cover_image image={notification.album.cover_image_url} />
            </div>
          </div>
        </li>
      </ul>
    </div>
    """
  end

  def handle_event("dismiss-notification", %{"id" => _id}, socket) do
    {:noreply, socket}
  end
end
