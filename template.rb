# Gems
# ==================================================

# For encrypted password
gem 'bcrypt-ruby'
# Useful SASS mixins (http://bourbon.io/)
gem 'bourbon'
gem 'rails_best_practices'

# HAML templating language (http://haml.info)
gem 'haml-rails'

# Simple form builder (https://github.com/plataformatec/simple_form)
#gem "simple_form"

gem_group :development do
  # Rspec for tests (https://github.com/rspec/rspec-rails)
  gem 'rspec-rails'
  # Guard for automatically launching your specs when files are modified. (https://github.com/guard/guard-rspec)
  gem "guard-rspec"

  gem 'better_errors'
  gem 'binding_of_caller'
  gem 'meta_request'
  gem 'rack-mini-profiler'
  gem 'letter_opener'
  gem 'quiet_assets'
end

gem_group :test do
  gem "rspec-rails"
  # Capybara for integration testing (https://github.com/jnicklas/capybara)
  gem "capybara"
  # FactoryGirl instead of Rails fixtures (https://github.com/thoughtbot/factory_girl)
  gem "factory_girl_rails"

  gem 'simplecov'
  gem 'shoulda'
end

# Run bundle isntall for installing unpresent gems
# ==================================================
run "bundle install"

# Initialize guard
# ==================================================
say "Initializing RSpec"
run "bundle exec guard init rspec"
run "bundle exec rails generate rspec:install"
run "rm -rf test/"

# Clean up Assets
# ==================================================
# Use SASS extension for application.css
say "Cleaning up assets."
run "mv app/assets/stylesheets/application.css app/assets/stylesheets/application.css.sass"
run "mv app/assets/javascripts/application.js app/assets/javascripts/application.js.coffee"
# Remove the require_tree directives from the SASS and JavaScript files.
# It's better design to import or require things manually.
run "sed -i '' /require_tree/d app/assets/javascripts/application.js"
run "sed -i '' /require_tree/d app/assets/stylesheets/application.css.sass"
# Add bourbon to stylesheet file
run "echo >> app/assets/stylesheets/application.css.sass"
run "echo '@import \"bourbon\";' >>  app/assets/stylesheets/application.css.sass"


# Font-awesome: Install from http://fortawesome.github.io/Font-Awesome/
# ==================================================
if yes?("Download font-awesome?")
  run "wget http://fortawesome.github.io/Font-Awesome/assets/font-awesome.zip -O font-awesome.zip"
  run "unzip font-awesome.zip && rm font-awesome.zip"
  run "cp font-awesome/css/font-awesome.css vendor/assets/stylesheets/"
  run "cp -r font-awesome/font public/font"
  run "rm -rf font-awesome"
  run "echo '@import \"font-awesome\";' >>  app/assets/stylesheets/application.css.sass"
end

# Ignore rails doc files, Vim/Emacs swap files, .DS_Store, and more
# ===================================================
run "echo '/.bundle' >> .gitignore"
run "echo '/db/*.sqlite3' >> .gitignore"
run "echo '/db/*.sqlite3-journal' >> .gitignore"
run "echo '/log/*.log' >> .gitignore"
run "echo '/tmp' >> .gitignore"
run "echo 'database.yml' >> .gitignore"
run "echo 'doc/' >> .gitignore"
run "echo '*.swp' >> .gitignore"
run "echo '*~' >> .gitignore"
run "echo '.project' >> .gitignore"
run "echo '.idea' >> .gitignore"
run "echo '.secret' >> .gitignore"
run "echo '.DS_Store' >> .gitignore"

# HAML: replace generated ERB for HAML and set the name
# ===================================================
say "Replacing ERB for HAML."
run "rm app/views/layouts/application.html.erb"
get 'https://raw.github.com/brianvanburken/rails_startup_template/master/application.html.haml', 'app/views/layouts/application.html.haml'
gsub_file 'app/views/layouts/application.html.haml', /App_Name/, "#{app_name.humanize.titleize}"

# Git: Initialize
# ==================================================
git :init
git add: "."
git commit: %Q{ -m 'Initial commit' }

if yes?("Initialize GitHub repository?")
  git_uri = `git config remote.origin.url`.strip
  unless git_uri.size == 0
    say "Repository already exists:"
    say "#{git_uri}"
  else
    username = ask "What is your GitHub username?"
    run "curl -u #{username} -d '{\"name\":\"#{app_name}\"}' https://api.github.com/user/repos"
    git remote: %Q{ add origin git@github.com:#{username}/#{app_name}.git }
    git push: %Q{ origin master }
  end
end
