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
    defaults [:read, :destroy]

    update :update do
      primary? true
      require_atomic? false
      accept [:name, :plan, :max_users, :active, :owner_id]
    end

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

    create :register do
      accept [:name, :slug]

      argument :owner, :map do
        allow_nil? false
      end

      change fn changeset, _ ->
        owner = Ash.Changeset.get_argument(changeset, :owner)

        changeset =
          case Ash.Changeset.get_attribute(changeset, :slug) do
            nil ->
              name = Ash.Changeset.get_attribute(changeset, :name) || ""

              slug =
                name
                |> String.downcase()
                |> String.replace(" ", "-")
                |> String.trim("-")

              Ash.Changeset.change_attribute(changeset, :slug, slug)

            _ ->
              changeset
          end

        changeset
        |> Ash.Changeset.after_action(fn _changeset, org ->
          user_params = %{
            email: owner[:email],
            password: owner[:password],
            password_confirmation: owner[:password_confirmation],
            organization_id: org.id
          }

          with {:ok, user} <-
                 Ash.create(TaskManager.Accounts.User, user_params,
                   action: :register_with_password,
                   authorize?: false
                 ),
               {:ok, _membership} <-
                 Ash.create(
                   TaskManager.Organizations.Membership,
                   %{
                     user_id: user.id,
                     organization_id: org.id,
                     role: :owner
                   },
                   tenant: org.id,
                   authorize?: false
                 ),
               {:ok, organization} <- Ash.update(org, %{owner_id: user.id}, authorize?: false) do
            {:ok, organization}
          else
            {:error, error} -> {:error, error}
          end
        end)
      end
    end
  end

  policies do
    bypass action(:register) do
      authorize_if always()
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

    attribute :owner_id, :uuid do
      public? true
      allow_nil? true
    end

    timestamps()
  end

  relationships do
    belongs_to :owner, TaskManager.Accounts.User do
      source_attribute :owner_id
      public? true
    end

    has_many :memberships, TaskManager.Organizations.Membership do
      public? true
    end

    many_to_many :users, TaskManager.Organizations.Membership do
      through TaskManager.Organizations.Membership
      destination_attribute_on_join_resource :user_id
      source_attribute_on_join_resource :organization_id
      public? true
    end
  end

  identities do
    identity :unique_slug, [:slug]
  end
end
