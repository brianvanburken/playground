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

require 'test_helper'

class AuthorTest < ActiveSupport::TestCase
  should validate_presence_of(:name)
  should validate_presence_of(:biography)
  should have_many(:taggings).dependent(:destroy)
  should have_many(:tags)

  test "tags words in biography" do
    tag = tags(:love)
    author = authors(:one)
    author.biography = 'I love this.'
    author.save
    assert_equal [tag], author.tags
  end

  test "finds words with between non-alphanumeric characters" do
    tag = tags(:love)
    author = authors(:one)
    author.biography = 'I say "love".'
    author.save
    assert_equal [tag], author.tags
  end

  test "ignores partial matched tags" do
    author = authors(:one)
    author.biography = 'I said "haters".'
    author.save
    assert author.tags.empty?
  end

  test "destroys old irrelevant tags" do
    love_tag = tags(:love)
    author = authors(:one)
    author.biography = 'I hate this.'
    author.save
    author.biography = 'I love this.'
    author.save
    author.reload
    assert_equal [love_tag], author.tags
  end
end
