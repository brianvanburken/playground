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
