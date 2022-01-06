class CreateLiteratures < ActiveRecord::Migration
  def change
    create_table :literatures do |t|
      t.string :title, null: false
      t.text :short_description
      t.text :content, null: false
      t.date :published_at, null: false

      t.timestamps null: false
    end
  end
end
