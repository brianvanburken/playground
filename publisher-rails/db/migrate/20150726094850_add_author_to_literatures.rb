class AddAuthorToLiteratures < ActiveRecord::Migration
  def change
    add_reference :literatures, :author, index: true, foreign_key: true
  end
end
