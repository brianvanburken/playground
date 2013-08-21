# Remove Rails 3/4 generated junk
remove_file "README"
remove_file "README.rdoc"
remove_file "doc/"
remove_file "public/index.html"
remove_file "public/images/rails.png"

# Cleanup Gemfile, by removing all commented lines
gsub_file "Gemfile", /#.*\n/, "\n"
gsub_file "Gemfile", /\n+/, "\n"

# Remove commented lines and multiple blank lines from config/routes.rb
gsub_file 'config/routes.rb', /  #.*\n/, "\n"
gsub_file 'config/routes.rb', /\n^\s*\n/, "\n"

# Add the config.generators in the file application.rb in config/
application do
  "config.generators do |generator|\n" +
  (" " * 4) + "end\n"
end
