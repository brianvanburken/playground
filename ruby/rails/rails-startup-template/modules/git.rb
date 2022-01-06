say("Going to initialize a blank repository", :white)

# Ignore rails doc files, Vim/Emacs swap files, .DS_Store, and more
get "#{File.dirname(__FILE__)}/../resources/.gitignore", ".gitignore", force: true

# Git: Initialize
git :init
git add: "."
git commit: %Q{ -m 'Initial commit' }


if yes?("Initialize GitHub repository?", :yellow)
  git_uri = `git config remote.origin.url`.strip
  unless git_uri.size == 0
    say "Repository already exists:"
    say "#{git_uri}"
  else
    username = ask "What is your GitHub username?"
    run "curl -u #{username} -d '{\"name\":\"#{app_name}\"}' https://api.github.com/user/repos"
    git remote: %Q{ add origin git@github.com:#{username}/#{app_name}.git }
    git push: %Q{ origin master }
  end
end
