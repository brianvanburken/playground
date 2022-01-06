# Normalize.css: Installed from https://github.com/necolas/normalize.css/
if yes?("Download normalize.css?", :yellow)
  get 'https://raw.github.com/necolas/normalize.css/master/normalize.css', 'vendor/assets/stylesheets/normalize.css'
  run "echo '@import \"normalize\"' >>  app/assets/stylesheets/application.css.sass"
end
