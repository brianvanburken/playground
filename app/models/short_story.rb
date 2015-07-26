# == Schema Information
#
# Table name: literatures
#
#  id                :integer          not null, primary key
#  title             :string           not null
#  short_description :text
#  content           :text             not null
#  published_at      :date             not null
#  created_at        :datetime         not null
#  updated_at        :datetime         not null
#  metaphor          :text
#  type              :string
#

class ShortStory < Literature
  validates :short_description, presence: true
end
