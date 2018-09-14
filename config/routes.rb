Rails.application.routes.draw do
  resources :tags
  root 'literatures#index'
  resources :poems
  resources :short_stories
  resources :authors
end
