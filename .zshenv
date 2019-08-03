# fpath=($fpath $HOME/.zsh/func)
# typeset -U fpath
source $HOME/.shrc

eval "$(/home/tj/dev/src/github.com/confluentinc/cc-dbmigrate/bin/dbmigrate init -)"

export CAAS_USER=travis
export CC_USER=travis
export GOPATH=$HOME/dev
export GOENV_ROOT=$HOME/.goenv
export PATH="$GOENV_ROOT/bin:$PATH"
export PATH="$GOPATH/bin:$PATH"

[ -d ~/.zsh/z ] && . ~/.zsh/z/z.sh

# for emacs vterm.el
HOSTNAME=$(uname -n)
USER=$(whoami)

case $TERM in
    xterm*)
        precmd () {print -Pn "\e]0;%n@%m: %~\a"}
        ;;
esac

eval "$(goenv init -)"
