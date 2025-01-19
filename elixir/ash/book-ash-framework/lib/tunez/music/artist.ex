defmodule Tunez.Music.Artist do
  use Ash.Resource, otp_app: :tunez, domain: Tunez.Music, data_layer: AshPostgres.DataLayer

  postgres do
    table "artists"
    repo Tunez.Repo
  end

  actions do
    defaults [:read, :destroy, :create]
    default_accept [:name, :biography]

    update :update do
      require_atomic? false

      accept [:name, :biography]

      change fn changeset, _context ->
               new_name = Ash.Changeset.get_attribute(changeset, :name)
               previous_name = Ash.Changeset.get_data(changeset, :name)
               previous_names = Ash.Changeset.get_data(changeset, :previous_names)

               names =
                 [previous_name | previous_names]
                 |> Enum.uniq()
                 |> Enum.reject(&(&1 == new_name))

               Ash.Changeset.change_attribute(changeset, :previous_names, names)
             end,
             where: [changing(:name)]
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
    end

    attribute :biography, :string

    attribute :previous_names, {:array, :string}, do: default([])

    create_timestamp :inserted_at
    create_timestamp :updated_at
  end

  relationships do
    has_many :albums, Tunez.Music.Album
  end
end
