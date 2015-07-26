Rails.application.routes.draw do
  root 'authors#index'
  resources :authors
end
