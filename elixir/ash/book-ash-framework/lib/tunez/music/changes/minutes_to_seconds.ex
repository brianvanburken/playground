defmodule Tunez.Music.Changes.MinutesToSeconds do
  use Ash.Resource.Change

  @impl true
  def change(changeset, _opts, _context) do
    {:ok, duration} = Ash.Changeset.fetch_argument(changeset, :duration)

    case Time.from_iso8601("00:" <> duration) do
      {:ok, %Time{minute: m, second: s}} ->
        total = m * 60 + s

        if total > 0 do
          Ash.Changeset.change_attribute(
            changeset,
            :duration_seconds,
            total
          )
        else
          Ash.Changeset.add_error(
            changeset,
            field: :duration,
            message: "must be at least 1 second long"
          )
        end

      {:error, _reason} ->
        Ash.Changeset.add_error(
          changeset,
          field: :duration,
          message: "use MM:SS format"
        )
    end
  end

  @impl true
  def atomic(changeset, opts, context) do
    {:ok, change(changeset, opts, context)}
  end
end
