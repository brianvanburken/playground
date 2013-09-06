if yes?("Install rspec and rspec-rails?", :yellow)
  # Remove Rails default test directory
  remove_file "test/"

  inject_into_file "config/application.rb", after: "config.generators do |generator|\n" do
    (" " * 6) + "generator.test_framework :rspec, views: false\n"
  end

  # Insert Gems into Gemfile
  gem_group :test do
    gem 'rspec-rails',      '~> 2.14.0'
    gem 'simplecov',        '~> 0.7.1'
    gem 'shoulda',          '~> 3.5.0'
    gem 'database_cleaner', '~> 1.1.1'
  end

  # Guard for automatically launching your specs when files are modified. (https://github.com/guard/guard-rspec)
  if yes?("Use Guard?", :yellow)
    gem "guard-rspec", '~> 3.0.2', group: :test

    unless gem_available?('guard-rspec', '~> 3.0.2')
      run "gem install guard-rspec -v '~> 3.0.2' --no-rdoc --no-ri"
    else
      say("Found database_cleaner, skipping installation", :green)
    end

    run "bundle exec guard init rspec"
  end

  # Check for existence of needed Gems
  unless gem_available?("rspec", "~> 2.14.0") and gem_available?("rspec-rails", "~> 2.14.0")
    run "gem install rspec -v '~> 2.14.0' --no-rdoc --no-ri"
    run "gem install rspec-rails -v '~> 2.14.0' --no-rdoc --no-ri"
  else
    say("Found rspec gem, skipping installation", :green)
    say("Found rspec-rails gems, skipping installation", :green)
  end

  unless gem_available?("database_cleaner", '~> 1.1.1')
    run "gem install database_cleaner -v '~> 1.1.1' --no-rdoc --no-ri"
  else
    say("Found database_cleaner, skipping installation", :green)
  end

  # Generate the RSpec files
  generate "rspec:install"

  # Setup DatabaseCleaner in spec_helper.rb
  append_file "spec/spec_helper.rb" do
    "\nDatabaseCleaner.strategy = :truncation"
  end

  # Install Mocha if wanted
  if yes?("Install mocha?", :yellow)
    gem 'mocha', group: :test

    append_file "spec/spec_helper.rb" do
      "Mocha::Configuration.warn_when(:stubbing_non_existent_method)\n" +
      "Mocha::Configuration.warn_when(:stubbing_non_public_method)"
    end

    gsub_file "spec/spec_helper.rb", /config\.mock_with :rspec/, "config.mock_with :mocha"
  end

  # Install Factory_Girl if wanted
  if yes?("Install factory_girl?", :yellow)
    gem 'factory_girl_rails', group: :test

    inject_into_file "config/application.rb", after: "config.generators do |generator|\n" do
      (" " * 6) + "generator.fixture_replacement :factory_girl, dir: '#{@use_rspec ? "spec/factories" : "test/factories"}'\n"
    end
    # TODO: inject require 'factory_girl' into spec_helper if @user_rspec
  end
end
