defmodule PentoWeb.WrongLive do
  use PentoWeb, :live_view
  alias Pento.Accounts

  def mount(_, session, socket) do
    user = Accounts.get_user_by_session_token(session["user_token"])
    new_socket = new_game(socket)

    {:ok,
     assign(
       new_socket,
       current_user: user,
       session_id: session["live_socket_id"]
     )}
  end

  def handle_params(%{"new_game" => "true"}, _, socket), do: {:noreply, new_game(socket)}
  def handle_params(_, _, socket), do: {:noreply, socket}

  def render(assigns) do
    ~H"""
    <h1 class="mb-4 text-4xl font-extrabold bg-">
      Your score: {@score}
    </h1>
    <h2>
      {@message}
    </h2>
    <br />
    <.link
      :if={@game_over}
      patch={~p"/guess?#{[new_game: true]}"}
      class="bg-cyan-500 hover:bg-cyan-700 text-white font-bold py-2 px-4 rounded"
    >
      restart
    </.link>
    <h2 :if={!@game_over}>
      <%= for n <- 1..10 do %>
        <.link
          class="bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 border border-blue-700 rounded m-1"
          phx-click="guess"
          phx-value-number={n}
        >
          {n}
        </.link>
      <% end %>
    </h2>
    <br />
    <pre>
      {@current_user.username || "anonymous"}
      {@session_id}
    </pre>
    """
  end

  def handle_event("guess", %{"number" => guess}, %{assigns: %{answer: guess, score: 4}} = socket) do
    score = socket.assigns.score + 1
    message = "You won! Starting a new game."
    {:noreply, assign(socket, score: score, message: message, game_over: true)}
  end

  def handle_event("guess", %{"number" => guess}, %{assigns: %{answer: guess}} = socket) do
    score = socket.assigns.score + 1
    message = "Correct! Try next number."
    answer = Enum.random(1..10) |> to_string()
    {:noreply, assign(socket, score: score, message: message, answer: answer)}
  end

  def handle_event("guess", %{"number" => guess}, socket) do
    score = socket.assigns.score - 1
    message = "Your guess: #{guess}. Wrong. Guess again."
    {:noreply, assign(socket, score: score, message: message)}
  end

  defp new_game(socket) do
    assign(
      socket,
      score: 0,
      message: "Make a guess:",
      answer: Enum.random(1..10) |> to_string(),
      game_over: false
    )
  end
end
