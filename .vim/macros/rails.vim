command! -nargs=0 Rroutes :find routes.rb
command! -nargs=0 RSroutes :Sfind routes.rb

command! -nargs=0 Rconfig :Rfind application.yml
command! -nargs=0 RSconfig :RSfind application.yml

command! -nargs=0 Rgemfile :e Gemfile
command! -nargs=0 RSgemfile :e Gemfile

command! -nargs=0 Rb :find blueprints.rb
command! -nargs=0 RSb :find blueprints.rb

Rnavcommand sass public/stylesheets/sass -suffix=.sass
