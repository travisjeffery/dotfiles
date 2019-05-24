# fpath=($fpath $HOME/.zsh/func)
# typeset -U fpath
source $HOME/.shrc
export GOPATH=$HOME/dev

eval "$(/home/tj/dev/src/github.com/confluentinc/cc-dbmigrate/bin/dbmigrate init -)"

source /home/tj/.gvm/scripts/gvm

export CAAS_USER=travis
export CC_USER=travis
