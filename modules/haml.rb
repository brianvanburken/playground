if yes?("Use HAML instead of ERB?", :yellow)

  # HAML templating language (http://haml.info)
  gem 'haml-rails'

  inside "app/views/layouts" do
    # Remove default generated layout
    remove_file "application.html.erb"

    # Place the HAML application template in layouts/
    get "#{File.dirname(__FILE__)}/../resources/application.html.haml", "application.html.haml"

    # Subtitude the placeholder for the application name
    gsub_file 'application.html.haml', /App_Name/, "#{app_name.humanize.titleize}"
  end

  # Set the generator for HAML as default
  inject_into_file "config/application.rb", after: "config.generators do |generator|\n" do
    (" " * 6) + "generator.template_engine :haml\n"
  end

  # Add output mode :ugly for HAML (for performance improvements) to the environment.rb
  append_file 'config/environment.rb' do
    "Haml::Template.options[:ugly] = true"
  end
end
