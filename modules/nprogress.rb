if yes?("Use NProgress.js for Turbolinks loading indicator?", :yellow)
  inside "vendor/assets/" do
    get 'https://raw.github.com/rstacruz/nprogress/master/nprogress.css', 'stylesheets/nprogress.css'
    get 'https://raw.github.com/rstacruz/nprogress/master/nprogress.js',  'javascripts/nprogress.js'
  end

  inside "app/assets/" do
    run "echo '@import \"nprogress\"' >>  stylesheets/application.css.sass"

    inject_into_file 'javascripts/application.js.coffee', after: "#= require turbolinks" do
      "\n#= require nprogress\n\n"
    end

    # Links NProgress with Turbolinks
    append_file "javascripts/application.js.coffee" do
"# Link Turbolink status changes with NProgress
NProgress.configure { showSpinner: false }
$(document).on 'page:fetch',   -> NProgress.start()
$(document).on 'page:change',  -> NProgress.done()
$(document).on 'page:restore', -> NProgress.remove()
"
    end

    # Adds the selectors for changing NProgress colours.
    append_file "stylesheets/application.css.sass" do
"
// Styles for colouring the NProgress plugin
$nprogress-colour: #29d
#nprogress .bar
  background: $nprogress-colour
#nprogress .peg
  box-shadow: 0 0 10px $nprogress-colour, 0 0 5px $nprogress-colour
#nprogress .spinner-icon
  border-top-color: $nprogress-colour
  border-left-color: $nprogress-colour
"
    end
  end

end
