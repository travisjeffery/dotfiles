# fpath=($fpath $HOME/.zsh/func)
# typeset -U fpath
source $HOME/.shrc
export GOPATH=$HOME/dev

eval "$(/home/tj/dev/src/github.com/confluentinc/cc-dbmigrate/bin/dbmigrate init -)"

export CAAS_USER=travis
export CC_USER=travis
export GOENV_ROOT=$HOME/.goenv
export PATH="$GOENV_ROOT/bin:$PATH"
export PATH="$GOPATH/bin:$PATH"

eval "$(goenv init -)"
