defmodule Tunez.Music.Album do
  use Ash.Resource,
    otp_app: :tunez,
    domain: Tunez.Music,
    data_layer: AshPostgres.DataLayer,
    extensions: [AshOban, AshGraphql.Resource, AshJsonApi.Resource],
    authorizers: [Ash.Policy.Authorizer]

  graphql do
    type :album
  end

  oban do
    triggers do
      trigger :send_new_album_notifications do
        action :send_new_album_notifications
        queue :default
        scheduler_cron false
        worker_module_name Tunez.Music.Album.AshOban.Worker.SendNewAlbumNotifications
        scheduler_module_name Tunez.Music.Album.AshOban.Scheduler.SendNewAlbumNotifications
      end
    end
  end

  json_api do
    type "album"
    includes [:tracks]
  end

  postgres do
    table "albums"
    repo Tunez.Repo

    references do
      reference :artist, index?: true
    end
  end

  actions do
    defaults [:read]

    create :create do
      accept [:name, :year_released, :cover_image_url, :artist_id]
      argument :tracks, {:array, :map}
      change manage_relationship(:tracks, type: :direct_control, order_is_key: :order)
    end

    update :update do
      accept [:name, :year_released, :cover_image_url]
      require_atomic? false
      argument :tracks, {:array, :map}
      change manage_relationship(:tracks, type: :direct_control, order_is_key: :order)
    end

    update :send_new_album_notifications do
      change Tunez.Accounts.Changes.SendNewAlbumNotifications
    end

    destroy :destroy do
      primary? true
      change cascade_destroy(:notifications, return_notifications?: true, after_action?: false)
    end
  end

  def next_year, do: Date.utc_today().year + 1

  policies do
    bypass AshOban.Checks.AshObanInteraction do
      authorize_if always()
    end

    bypass actor_attribute_equals(:role, :admin) do
      authorize_if always()
    end

    policy action_type(:read) do
      authorize_if always()
    end

    policy action_type(:create) do
      authorize_if actor_attribute_equals(:role, :editor)
    end

    policy action_type([:update, :destroy]) do
      authorize_if expr(can_manage_album?)
    end
  end

  changes do
    change run_oban_trigger(:send_new_album_notifications), on: [:create]
    change relate_actor(:created_by, allow_nil?: true), on: [:create]
    change relate_actor(:updated_by, allow_nil?: true)

    change Tunez.Accounts.Changes.SendNewAlbumNotifications, on: [:create]
  end

  validations do
    validate numericality(:year_released,
               greater_than: 1950,
               less_than_or_equal_to: &__MODULE__.next_year/0
             ),
             where: [present(:year_released)],
             message: "must be between 1950 and next year"

    validate match(
               :cover_image_url,
               ~r"(^https://|/images/).+\.(png|jpg|jpeg)$"
             ),
             where: [changing(:cover_image_url)],
             message: "must start with https:// or /images/"
  end

  attributes do
    uuid_primary_key :id

    attribute :name, :string do
      allow_nil? false
      public? true
    end

    attribute :year_released, :integer do
      allow_nil? false
      public? true
    end

    attribute :cover_image_url, :string do
      public? true
    end

    create_timestamp :inserted_at
    update_timestamp :updated_at
  end

  relationships do
    belongs_to :artist, Tunez.Music.Artist do
      allow_nil? false
    end

    belongs_to :created_by, Tunez.Accounts.User
    belongs_to :updated_by, Tunez.Accounts.User

    has_many :tracks, Tunez.Music.Track do
      sort order: :asc
      public? true
    end

    has_many :notifications, Tunez.Accounts.Notification
  end

  calculations do
    calculate :years_ago, :integer, expr(2025 - year_released)

    calculate :string_years_ago,
              :string,
              expr("wow, this was released " <> years_ago <> " years ago!")

    calculate :can_manage_album?,
              :boolean,
              expr(
                ^actor(:role) == :admin or
                  (^actor(:role) == :editor and created_by_id == ^actor(:id))
              )
  end

  calculations do
    calculate :duration, :string, Tunez.Music.Calculations.SecondsToMinutes
  end

  aggregates do
    sum :duration_seconds, :tracks, :duration_seconds
  end

  identities do
    identity :unique_album_names_per_artist, [:name, :artist_id],
      message: "already exists for this artist"
  end
end
