Rails.application.routes.draw do
  root 'authors#index'
  resources :poems
  resources :short_stories
  resources :authors
end
