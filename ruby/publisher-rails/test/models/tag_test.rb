# == Schema Information
#
# Table name: tags
#
#  id         :integer          not null, primary key
#  name       :string           not null
#  created_at :datetime         not null
#  updated_at :datetime         not null
#

require 'test_helper'

class TagTest < ActiveSupport::TestCase
  should validate_presence_of(:name)
  should validate_uniqueness_of(:name)
  should allow_value('hello').for(:name)
  should allow_value('h3110').for(:name)
  should_not allow_value('hello-world').for(:name)
  should_not allow_value('hello world').for(:name)
  should_not allow_value('hello.').for(:name)
  should have_many(:taggings).dependent(:destroy)

  test "downcase name" do
    tag = Tag.new
    tag.name = 'HeLLo'
    tag.save
    assert_equal 'hello', tag.name
  end
end
