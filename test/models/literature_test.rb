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

class LiteratureTest < ActiveSupport::TestCase
  should validate_presence_of(:title)
  should validate_presence_of(:content)
  should validate_presence_of(:published_at)
  should validate_presence_of(:author)
  should belong_to(:author)
  should have_many(:taggings).dependent(:destroy)
  should have_many(:tags)

  test "published_at does not accept dates in the future" do
    literature = literatures(:one)
    literature.published_at = 1.day.from_now
    assert_not literature.valid?
    assert literature.errors.include?(:published_at)
    assert_equal ["can't be in the future"], literature.errors.get(:published_at)
  end

  test "published_at only accepts dates in the past" do
    literature = literatures(:one)
    literature.published_at = 1.day.ago
    assert literature.valid?
    assert_not literature.errors.include?(:published_at)
  end

  test "tags words in content" do
    tag = tags(:love)
    literature = literatures(:one)
    literature.content = 'I love this.'
    literature.save
    assert_equal [tag], literature.tags
  end

  test "finds words with between non-alphanumeric characters" do
    tag = tags(:love)
    literature = literatures(:one)
    literature.content = 'I say "love".'
    literature.save
    assert_equal [tag], literature.tags
  end

  test "ignores partial matched tags" do
    literature = literatures(:one)
    literature.content = 'I said "haters".'
    literature.save
    assert literature.tags.empty?
  end

  test "destroys old irrelevant tags" do
    love_tag = tags(:love)
    literature = literatures(:one)
    literature.content = 'I hate this.'
    literature.save
    literature.content = 'I love this.'
    literature.save
    literature.reload
    assert_equal [love_tag], literature.tags
  end
end
