if yes?("Use NProgress.js for Turbolinks loading indicator?", :yellow)
  inside "vendor/assets/" do
    get 'https://raw.github.com/rstacruz/nprogress/master/nprogress.css', 'stylesheets/nprogress.css'
    get 'https://raw.github.com/rstacruz/nprogress/master/nprogress.js',  'javascripts/nprogress.js'
  end

  inside "app/assets/" do
    run "echo '@import \"nprogress\"' >>  stylesheets/application.css.sass"

    inject_into_file 'javascripts/application.js.coffee', after: "#= require turbolinks" do
      "\n#= require nprogress\n"
    end

    # Links NProgress with Turbolinks
    append_file "javascripts/application.js.coffee" do
      "\n" +
      "# Link Turbolink status changes with NProgress\n" +
      "NProgress.configure { showSpinner: false }\n" +
      "$(document).on 'page:fetch',   -> NProgress.start()\n" +
      "$(document).on 'page:change',  -> NProgress.done()\n" +
      "$(document).on 'page:restore', -> NProgress.remove()\n"
    end

    # Adds the selectors for changing NProgress colours.
    append_file "stylesheets/application.css.sass" do
      "\n" +
      "// Styles for colouring the NProgress plugin\n" +
      "$nprogress-colour: #29d\n" +
      "#nprogress .bar\n" +
      "  background: $nprogress-colour\n" +
      "#nprogress .peg\n" +
      "  box-shadow: 0 0 10px $nprogress-colour, 0 0 5px $nprogress-colour\n" +
      "#nprogress .spinner-icon\n" +
      "  border-top-color: $nprogress-colour\n" +
      "  border-left-color: $nprogress-colour\n"
    end
  end

end
