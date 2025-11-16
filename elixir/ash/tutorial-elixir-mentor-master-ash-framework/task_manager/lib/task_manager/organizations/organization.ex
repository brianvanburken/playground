defmodule TaskManager.Organizations.Organization do
  use Ash.Resource,
    otp_app: :task_manager,
    domain: TaskManager.Organizations,
    data_layer: AshPostgres.DataLayer,
    authorizers: [Ash.Policy.Authorizer],
    extensions: [AshAuthentication]

  postgres do
    table "organizations"
    repo TaskManager.Repo
  end

  actions do
    defaults [:read, :update, :destroy]

    create :create do
      primary? true
      accept [:name, :slug, :plan, :max_users]

      change fn changeset, _ ->
        Ash.Changeset.before_action(changeset, fn changeset ->
          if Ash.Changeset.get_attribute(changeset, :slug) do
            changeset
          else
            name = Ash.Changeset.get_attribute(changeset, :name) || ""

            slug =
              name
              |> String.downcase()
              |> String.replace(" ", "-")
              |> String.trim("-")

            Ash.Changeset.change_attribute(changeset, :slug, slug)
          end
        end)
      end
    end
  end

  validations do
    validate match(:slug, ~r/^[a-z0-9-]+$/) do
      message "must only contain lowercase letters, numbers, and hyphens"
    end

    validate string_length(:name, min: 2, max: 100)
    validate string_length(:slug, min: 2, max: 100)
  end

  attributes do
    uuid_primary_key :id
    attribute :name, :string
    attribute :slug, :string
    attribute :plan, :atom
    attribute :max_users, :integer
    attribute :active, :boolean
    timestamps()
  end

  identities do
    identity :unique_slug, [:slug]
  end
end
