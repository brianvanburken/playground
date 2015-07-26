Rails.application.routes.draw do
  root 'literatures#index'
  resources :poems
  resources :short_stories
  resources :authors
end
