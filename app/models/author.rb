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
  include Taggable
  validates :name, :biography, presence: true

  after_save :tag_biography

  private
    def tag_biography
      words = biography.strip.downcase.split(/\W+/)
      TaggingService.new(self).tag(words)
    end
end
