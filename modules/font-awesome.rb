# Font-awesome: Installed from http://fortawesome.github.io/Font-Awesome/
if yes?("Download font-awesome?", :yellow)
  get 'https://github.com/FortAwesome/Font-Awesome/archive/master.zip', 'font-awesome.zip'
  run "unzip font-awesome.zip && rm font-awesome.zip"
  run "cp Font-Awesome-master/css/font-awesome.min.css vendor/assets/stylesheets/font-awesome.css"
  run "cp -r Font-Awesome-master/fonts public/fonts"
  run "rm -rf Font-Awesome-master"
  run "rm public/fonts/FontAwesome.otf"
  run "echo '@import \"font-awesome\"' >>  app/assets/stylesheets/application.css.sass"
end
