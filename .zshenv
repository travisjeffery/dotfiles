source $HOME/.shrc
source $HOME/work.sh

export GOPATH=$HOME/dev
export GOENV_ROOT=$HOME/.goenv
export PATH="$GOENV_ROOT/bin:$PATH"
export PATH="$GOPATH/bin:$PATH"

[ -d ~/.zsh/z ] && . ~/.zsh/z/zsh-z.plugin.zsh

# for emacs vterm.el
HOSTNAME=$(uname -n)
USER=$(whoami)

case $TERM in
    xterm*)
        precmd () {print -Pn "\e]0;%n@%m: %~\a"}
        ;;
esac

if type goenv &> /dev/null; then
  eval "$(goenv init -)"
fi
