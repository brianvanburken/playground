if yes?("Use nprogress for Turbolinks loading indicator?", :yellow)
  inside "vendor" do
    get 'https://raw.github.com/rstacruz/nprogress/master/nprogress.css', 'stylesheets/nprogress.css'
    get 'https://raw.github.com/rstacruz/nprogress/master/nprogress.js',  'javascripts/nprogress.js'
  end
  run "echo '@import \"nprogress\"' >>  app/assets/stylesheets/application.css.sass"
  inject_into_file 'app/assets/javascripts/application.js.coffee' do
    "#= require nprogress\n\n"+
    "$(document).on 'page:fetch',   -> NProgress.start()\n" +
    "$(document).on 'page:change',  -> NProgress.done()\n" +
    "$(document).on 'page:restore', -> NProgress.remove()\n"
  end
end
