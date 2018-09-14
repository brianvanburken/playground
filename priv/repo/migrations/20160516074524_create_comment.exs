defmodule BlogPhoenix.Repo.Migrations.CreateComment do
  use Ecto.Migration

  def change do
    create table(:comments) do
      add :name, :string
      add :content, :text
      add :post_id, references(:posts, on_delete: :nothing)

      timestamps
    end
    create index(:comments, [:post_id])

  end
end
