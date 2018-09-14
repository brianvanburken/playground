class CreateTaggings < ActiveRecord::Migration
  def change
    create_table :taggings do |t|
      t.string :taggable_type
      t.integer :taggable_id, index: true, foreign_key: true
      t.belongs_to :tag, index: true, foreign_key: true

      t.timestamps null: false
    end
  end
end
