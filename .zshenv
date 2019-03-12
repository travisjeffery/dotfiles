# fpath=($fpath $HOME/.zsh/func)
# typeset -U fpath
source $HOME/.shrc
export GOPATH=$HOME/dev

eval "$(/home/tj/dev/src/github.com/confluentinc/cc-dbmigrate/bin/dbmigrate init -)"
