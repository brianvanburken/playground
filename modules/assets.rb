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

# Normalize.css: Installed from https://github.com/necolas/normalize.css/
if yes?("Download normalize.css?", :yellow)
  get 'https://raw.github.com/necolas/normalize.css/master/normalize.css', 'vendor/assets/stylesheets/normalize.css'
  run "echo '@import \"normalize\"' >>  app/assets/stylesheets/application.css.sass"
end

# Font-awesome: Installed from http://fortawesome.github.io/Font-Awesome/
if yes?("Download font-awesome?", :yellow)
  say "Getting font-awesome"
  run "wget http://fortawesome.github.io/Font-Awesome/assets/font-awesome.zip -O font-awesome.zip"
  run "unzip font-awesome.zip && rm font-awesome.zip"
  run "cp font-awesome/css/font-awesome.css vendor/assets/stylesheets/"
  run "cp -r font-awesome/font public/font"
  run "rm -rf font-awesome"
  run "echo '@import \"font-awesome\"' >>  app/assets/stylesheets/application.css.sass"
end

# Set the generator for SASS as default instead of SCSS
inject_into_file "config/application.rb", after: "config.generators do |generator|\n" do
  (" " * 6) + "generator.stylesheet_engine :sass\n"
end
