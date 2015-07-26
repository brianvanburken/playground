class TaggingService
  def initialize(record)
    @record = record
  end

  def tag(words)
    @record.taggings.delete_all
    Tag.where(name: words).each do |tag|
      @record.tags << tag
    end
  end
end
