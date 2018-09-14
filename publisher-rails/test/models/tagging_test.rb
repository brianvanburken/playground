# == Schema Information
#
# Table name: taggings
#
#  id            :integer          not null, primary key
#  taggable_type :string
#  taggable_id   :integer
#  tag_id        :integer
#  created_at    :datetime         not null
#  updated_at    :datetime         not null
#
# Indexes
#
#  index_taggings_on_tag_id       (tag_id)
#  index_taggings_on_taggable_id  (taggable_id)
#

require 'test_helper'

class TaggingTest < ActiveSupport::TestCase
  should belong_to(:tag)
  should belong_to(:taggable)
end
