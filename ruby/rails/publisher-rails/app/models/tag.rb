# == Schema Information
#
# Table name: tags
#
#  id         :integer          not null, primary key
#  name       :string           not null
#  created_at :datetime         not null
#  updated_at :datetime         not null
#

class Tag < ActiveRecord::Base
  has_many :taggings, dependent: :destroy
  before_save :downcase_name
  validates :name, presence: true, uniqueness: true, format: {
    with: /\A[a-zA-Z0-9]+\z/,
    message: "only allows alphanumeric"
  }

  private
    def downcase_name
      name.downcase! if name.present?
    end

end
