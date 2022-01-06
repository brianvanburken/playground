class LiteraturesController < ApplicationController
  def index
    @literature = Literature.all
  end
end
