Rails.application.routes.draw do
  root 'authors#index'
  resources :poems
  resources :authors
end
