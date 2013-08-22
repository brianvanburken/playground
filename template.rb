@template_path = File.dirname(__FILE__)

# Method to catch Gem available not defined error
def gem_available?(gemname, version = nil)
  if Gem::Specification.methods.include?(:find_all_by_name)
    not Gem::Specification.find_all_by_name(gemname, version).empty?
  else
    Gem.available?(gemname, version)
  end
end

apply "#{@template_path}/modules/cleanup.rb"
apply "#{@template_path}/modules/haml.rb"
apply "#{@template_path}/modules/database.rb"
apply "#{@template_path}/modules/rspec.rb"
apply "#{@template_path}/modules/gems.rb"
apply "#{@template_path}/modules/assets.rb"
apply "#{@template_path}/modules/normalize.rb"
apply "#{@template_path}/modules/font-awesome.rb"
apply "#{@template_path}/modules/nprogress.rb"
apply "#{@template_path}/modules/git.rb"
