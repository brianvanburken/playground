class AddTypeToLiteratures < ActiveRecord::Migration
  def change
    add_column :literatures, :type, :string
  end
end
