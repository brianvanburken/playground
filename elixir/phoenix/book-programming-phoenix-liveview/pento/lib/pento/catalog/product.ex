defmodule Pento.Catalog.Product do
  use Ecto.Schema
  import Ecto.Changeset

  schema "products" do
    field(:name, :string)
    field(:description, :string)
    field(:unit_price, :float)
    field(:sku, :integer)

    timestamps(type: :utc_datetime)
  end

  @doc false
  def changeset(product, attrs) do
    product
    |> cast(attrs, [:name, :description, :unit_price, :sku])
    |> validate_required([:name, :description, :unit_price, :sku])
    |> unique_constraint(:sku)
    |> validate_number(:unit_price, greater_than: 0.0)
  end

  def changeset_markdown_product(product, attrs) do
    product
    |> cast(attrs, [:unit_price])
    |> validate_number(:unit_price,
      less_than: product.unit_price,
      message: "new price must be lower than current price"
    )
  end
end
