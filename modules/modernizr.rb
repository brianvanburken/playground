# http://modernizr.com/downloads/modernizr-latest.js
if yes?("Download Modernizr?", :yellow)
  get 'http://modernizr.com/downloads/modernizr-latest.js', 'vendor/assets/javascripts/modernizr.js'
  inject_into_file 'app/assets/javascripts/application.js.coffee',
                    after: "#= require turbolinks" do
    "\n#= require modernizr\n\n"
  end
end
