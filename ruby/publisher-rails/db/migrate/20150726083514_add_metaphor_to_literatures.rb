class AddMetaphorToLiteratures < ActiveRecord::Migration
  def change
    add_column :literatures, :metaphor, :text
  end
end
