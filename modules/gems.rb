# Useful SASS mixins (http://bourbon.io/)
gem 'bourbon'

# Checks if we're still following the best practices and picks out bad smells
gem 'rails_best_practices'

# Simple pagination gem (https://github.com/amatsuda/kaminari)
gem 'kaminari', '~> 0.14.1'

# Simple form builder (https://github.com/plataformatec/simple_form)
#gem "simple_form", '~> 2.1.0'

gem_group :development do
  gem 'binding_of_caller', '~> 0.7.2' # Needed for better_errors
  gem 'better_errors', '~> 0.9.0'

  # Rack Mini-Profiler for catching performance bottlenecks early stage
  gem 'rack-mini-profiler', '~> 0.1.29'

  # Opens email in browser instead of sending it
  gem 'letter_opener', '~> 1.1.2'

  # Silence those annoying assests pipeline loglines
  gem 'quiet_assets', '~> 1.0.2'
end
