defmodule Tunez.Music.Artist do
  use Ash.Resource, otp_app: :tunez, domain: Tunez.Music, data_layer: AshPostgres.DataLayer

  postgres do
    table "artists"
    repo Tunez.Repo

    custom_indexes do
      index "name gin_trgm_ops", name: "artist_name_gin_index", using: "GIN"
    end
  end

  actions do
    defaults [:read, :destroy, :create]
    default_accept [:name, :biography]

    update :update do
      require_atomic? false

      accept [:name, :biography]

      change Tunez.Music.Changes.UpdatePreviousNames,
        where: [changing(:name)]
    end

    read :search do
      argument :query, :ci_string do
        constraints allow_empty?: true
        default ""
      end

      filter expr(contains(name, ^arg(:query)))
    end
  end

  changes do
    change set_attribute(:inserted_at, &DateTime.utc_now/0), on: [:create]
    change set_attribute(:updated_at, &DateTime.utc_now/0)
  end

  attributes do
    uuid_primary_key :id

    attribute :name, :string do
      allow_nil? false
      public? true
    end

    attribute :biography, :string

    attribute :previous_names, {:array, :string}, do: default([])

    create_timestamp :inserted_at, public?: true
    create_timestamp :updated_at, public?: true
  end

  relationships do
    has_many :albums, Tunez.Music.Album
  end
end
