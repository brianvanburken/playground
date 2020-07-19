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
#  author_id         :integer
#
# Indexes
#
#  index_literatures_on_author_id  (author_id)
#

require 'test_helper'

class ShortStoryTest < ActiveSupport::TestCase
  should validate_presence_of(:short_description)
end
