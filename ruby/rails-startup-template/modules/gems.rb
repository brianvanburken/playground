# Checks if we're still following the best practices and picks out bad smells
gem 'rails_best_practices'

if yes?("Use kaminari pagination?", :yellow)
  # Simple pagination gem (https://github.com/amatsuda/kaminari)
  gem 'kaminari'
end

if yes?("Use bourbon library?", :yellow)
  gem 'bourbon'
  # Add bourbon to stylesheet
  # Useful SASS mixins (http://bourbon.io/)
  append_file "app/assets/stylesheets/application.css.sass" do
    "@import \"bourbon\"\n"
  end
end

if yes?("Use simple_form builder?", :yellow)
  # Simple form builder (https://github.com/plataformatec/simple_form)
  gem 'simple_form'
end

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
