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

require 'test_helper'

class LiteratureTest < ActiveSupport::TestCase
  # test "the truth" do
  #   assert true
  # end
end
