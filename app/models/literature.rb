# == Schema Information
#
# Table name: literatures
#
#  id                :integer          not null, primary key
#  title             :string           not null
#  short_description :text             not null
#  content           :text             not null
#  published_at      :date             not null
#  created_at        :datetime         not null
#  updated_at        :datetime         not null
#

class Literature < ActiveRecord::Base
  validates :title, :short_description, :content, :published_at, presence: true
  validate :published_in_the_past

  private
    def published_in_the_past
      if published_at && published_at > Date.today
        errors.add(:published_at, "can't be in the future")
      end
    end
end
