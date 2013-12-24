if yes?("Use Cucumber behavior testing?", :yellow)

  gem_group :test do
    gem 'database_cleaner', '~> 1.1.1'
    gem 'cucumber-rails',   '~> 1.4.0'
    gem 'launchy',          '~> 2.3.0'
  end

  if yes?("Use Capybara instead of Webrat?", :yellow)
    gem 'capybara', '~> 2.1.0', group: :test
    @use_capybara = true
    unless gem_available?("capybara")
      run "gem install capybara -v '~> 2.1.0' --no-rdoc --no-ri"
    else
      say("Found capybara gem, skipping installation", :green)
    end
  else
    gem 'webrat', '~> 0.7.3', group: :test
    unless gem_available?("webrat")
      run "gem install webrat -v '~> 0.7.3' --no-rdoc --no-ri"
    else
      say("Found webrat gem, skipping installation", :green)
    end
  end

  unless gem_available?("cucumber-rails")
    run "gem install cucumber-rails -v '~> 1.4.0' --no-rdoc --no-ri"
  else
    say("Found cucumber-rails, skipping installation", :green)
  end

  unless gem_available?("database_cleaner")
    run "gem install database_cleaner -v '~> 1.1.1' --no-rdoc --no-ri"
  else
    say("Found database_cleaner, skipping installation", :green)
  end

  unless gem_available?("launchy")
    run "gem install launchy -v '~> 2.3.0' --no-rdoc --no-ri"
  else
    say("Found launchy gem, skipping installation", :green)
  end

  arguments = [].tap do |arguments|
    arguments << "--webrat"    if @use_capybara.nil?
    arguments << "--capybara"  if @use_capybara.present?
    arguments << "--rspec"     if yes?("Use Cucumber with RSpec?", :yellow)
  end

  generate "cucumber:install #{arguments.join(" ")}"

  get "#{@template_path}/resources/cucumber.yml", "config/cucumber.yml", force: true

end
