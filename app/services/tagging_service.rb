class TaggingService
  def initialize(record)
    @record = record
  end

  def tag(words)
    @record.taggings.delete_all
    Tag.all.each do |tag|
      @record.tags << tag if words.include? tag.name
    end
  end
end
