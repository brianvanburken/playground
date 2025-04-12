defmodule Tunez.Music do
  use Ash.Domain,
    otp_app: :tunez

  resources do
    resource Tunez.Music.Artist do
      define :create_artist, action: :create
      define :read_artist, action: :read
      define :get_artist_by_id, action: :read, get_by: :id
    end
  end
end
