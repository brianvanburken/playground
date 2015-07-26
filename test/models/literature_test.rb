# == Schema Information
#
# Table name: literatures
#
#  id                :integer          not null, primary key
#  title             :string
#  short_description :text
#  content           :text
#  published_at      :date
#  created_at        :datetime         not null
#  updated_at        :datetime         not null
#

require 'test_helper'

class LiteratureTest < ActiveSupport::TestCase
  # test "the truth" do
  #   assert true
  # end
end
