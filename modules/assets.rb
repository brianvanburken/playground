inside "app/assets" do

  remove_file "stylesheets/application.css"
  create_file "stylesheets/application.css.sass", force: true
  # Add bourbon to stylesheet
  append_file "stylesheets/application.css.sass" do
    "@import \"bourbon\"\n"
  end

  # Remove the require_tree directives from the JavaScript file.
  # It's better design to import or require things manually.
  run "sed -i '' /require_tree/d javascripts/application.js"
  run "mv javascripts/application.js javascripts/application.js.coffee"
  gsub_file 'javascripts/application.js.coffee', /\/\//, '#'
end

# Set the generator for SASS as default instead of SCSS
inject_into_file "config/application.rb", after: "config.generators do |generator|\n" do
  (" " * 6) + "generator.stylesheet_engine :sass\n"
end
