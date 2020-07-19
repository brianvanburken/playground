class TaggingService
  def initialize(record)
    @record = record
  end

  def tag(words)
    @record.taggings.delete_all
    @record.tags << Tag.where(name: words)
  end
end
