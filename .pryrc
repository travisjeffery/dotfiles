# -*- mode: ruby -*- vim:set ft=ruby:

Pry.config.editor = "vim"

# Prompt with ruby version
# Pry.prompt = [proc { |obj, nest_level, _| "#{RUBY_VERSION} (#{obj}):#{nest_level} > " }, proc { |obj, nest_level, _| "#{RUBY_VERSION} (#{obj}):#{nest_level} * " }]

Pry.config.history.file = if defined?(Bundler)
                            Bundler.tmp.parent.join('history.rb')
                          else
                            File.expand_path('~/.history.rb')
                          end

cs=Pry::CommandSet.new do
  import Pry::Commands
  command "lm","Alias for ls -m" do |args|
   run "ls", "-m #{args}"
  end
  command "lM", "Alias for ls -M" do |args|
   run "ls", "-M #{args}"
  end
end

Pry.config.commands = cs

# loading rails configuration if it is running as a rails console
load File.dirname(__FILE__) + '/.railsrc' if defined?(Rails) && Rails.env

$LOAD_PATH.unshift(File.expand_path('~/.ruby/lib'), File.expand_path('~/.ruby'))
$LOAD_PATH.uniq!
