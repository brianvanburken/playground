# Checks if we're still following the best practices and picks out bad smells
gem 'rails_best_practices'

# Simple pagination gem (https://github.com/amatsuda/kaminari)
gem 'kaminari', '~> 0.14.1'

# Useful SASS mixins (http://bourbon.io/)
gem 'bourbon'

# Simple form builder (https://github.com/plataformatec/simple_form)
#gem "simple_form", '~> 2.1.0'

gem_group :development do
  gem 'binding_of_caller' # Needed for better_errors
  gem 'better_errors'
  # Opens email in browser instead of sending it
  gem 'letter_opener'
  # Silence those annoying assests pipeline loglines
  gem 'quiet_assets'
  # Middleware that displays speed badge for every html page.
  gem 'rack-mini-profiler'
  # Gem for detecting N+1 and other performance improvements
  gem 'bullet'
  # Brakeman is a static analysis tool which checks Ruby on Rails applications for security vulnerabilities.
  gem 'brakeman'
end
