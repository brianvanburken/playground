class ShortStoriesController < ApplicationController
  before_action :set_short_story, only: [:show, :edit, :update, :destroy]

  # GET /short_stories
  def index
    @short_stories = ShortStory.all
  end

  # GET /short_stories/1
  def show
  end

  # GET /short_stories/new
  def new
    @short_story = ShortStory.new
  end

  # GET /short_stories/1/edit
  def edit
  end

  # POST /short_stories
  def create
    @short_story = ShortStory.new(short_story_params)
    if @short_story.save
      redirect_to @short_story, notice: 'Short story was successfully created.'
    else
      render :new
    end
  end

  # PATCH/PUT /short_stories/1
  def update
    if @short_story.update(short_story_params)
      redirect_to @short_story, notice: 'Short story was successfully updated.'
    else
      render :edit
    end
  end

  # DELETE /short_stories/1
  def destroy
    @short_story.destroy
    redirect_to short_stories_url, notice: 'Short story was successfully destroyed.'
  end

  private
    # Use callbacks to share common setup or constraints between actions.
    def set_short_story
      @short_story = ShortStory.find(params[:id])
    end

    # Never trust parameters from the scary internet, only allow the white list through.
    def short_story_params
      params.require(:short_story).permit(:title, :content, :short_description,
                                          :published_at, :author_id)
    end
end
