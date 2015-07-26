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
  before_save :downcase_name
  validates :name, presence: true, uniqueness: true, format: {
    with: /\A[a-zA-Z0-9\-]+\z/,
    message: "only allows alphanumeric and hypen"
  }

  private
    def downcase_name
      name.downcase! if name.present?
    end

end
