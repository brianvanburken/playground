if yes?("Use nprogress for Turbolinks loading indicator?", :yellow)
  inside "vendor" do
    get 'https://raw.github.com/rstacruz/nprogress/master/nprogress.css', 'stylesheets/nprogress.css'
    get 'https://raw.github.com/rstacruz/nprogress/master/nprogress.js',  'javascripts/nprogress.js'
  end

  run "echo '@import \"nprogress\"' >>  app/assets/stylesheets/application.css.sass"

  inject_into_file 'app/assets/javascripts/application.js.coffee', after: "#= require turbolinks" do
    "\n#= require nprogress\n\n"
  end

  # Links NProgress with Turbolinks
  append_file "app/assets/javascripts/application.js.coffee" do
    "\n" +
    "# Link Turbolink status changes with NProgress \n" +
    "$(document).on 'page:fetch',   -> NProgress.start()\n" +
    "$(document).on 'page:change',  -> NProgress.done()\n" +
    "$(document).on 'page:restore', -> NProgress.remove()\n"
  end

  # Adds the selectors for changing NProgress colours.
  append_file "app/assets/stylesheets/application.css.sass" do
    "\n" +
    "// Styles for NProgress plugin \n" +
    "#nprogress .bar { background: #29d; }\n" +
    "#nprogress .peg { box-shadow: 0 0 10px #29d, 0 0 5px #29d; }\n" +
    "#nprogress .spinner-icon { border-top-color: #29d; border-left-color: #29d; }\n"
  end

end
