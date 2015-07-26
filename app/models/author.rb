# == Schema Information
#
# Table name: authors
#
#  id         :integer          not null, primary key
#  name       :string           not null
#  biography  :text             not null
#  created_at :datetime         not null
#  updated_at :datetime         not null
#

class Author < ActiveRecord::Base
  has_many :taggings, as: :taggable
  has_many :tags, through: :taggings
  validates :name, :biography, presence: true
end
