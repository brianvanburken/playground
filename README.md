# Rails Startup template

This is a template I use for my new Ruby on Rails 4 applications. **Pull requests are welcome.**

## How to Use

```bash
rails new [app_name] -m https://github.com/brianvanburken/rails_startup_template/template.rb
```

## What it does

1. Adds the following gems:
  - bcrypt-ruby: I usually implement authentication myself instead of using gems like Devise. This is needed for the `has_secure_password` functionality. [See API Doc](http://api.rubyonrails.org/classes/ActiveModel/SecurePassword/ClassMethods.html).
  - [kaminari](https://github.com/amatsuda/kaminari): Easy to use and customizable pagination gem.
  - [bourbon](http://bourbon.io/): Bourbon provides useful SASS mixins for cross-browser compatibility.
  - [haml-rails](http://haml.info): HAML is a beautiful templating language. I prefer it over ERB.
  - (development environment) [better_errors](https://github.com/charliesome/better_errors): Replaces the default error page with more information for better debugging.
  - (development environment) [letter_opener](https://github.com/ryanb/letter_opener): Preview emails in the browser instead of sending. And no accidental sending emails in development ;-)
  - (development environment) [rack-mini-profiler](https://github.com/SamSaffron/MiniProfiler/tree/master/Ruby): Displays the speed of the application in detail so possible bottlenecks can be filtered in early stage.
  - [rspec-rails](https://github.com/rspec/rspec-rails): Rspec is a testing tool for test-driven and behavior-driven development. It makes writing specs more enjoyable.
  - [guard-rspec](https://github.com/guard/guard-rspec): Guard for automatically launching your specs when files are modified.
  - (test environment) [capybara](https://github.com/jnicklas/capybara): I use Capybara to write integration tests and simulate user behavior.
  - (test environment) [factory_girl_rails](https://github.com/thoughtbot/factory_girl): FactoryGirl provdes a flexible alternative to Rails fixtures.

2. Sets up RSpec and Gaurd.

3. Cleans up assets by renaming `application.css` to `application.css.scss` and removing the `include_tree` directives. It's better design to import and require things manually. For example, `@import 'bourbon';`

4. Changes the ERB files to HAML version

5. Optionally installs [Font Awesome](http://fortawesome.github.io/Font-Awesome/).

6. Initializes a new git repository with an initial commit.

7. Optionally create a github repository.
