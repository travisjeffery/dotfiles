source $HOME/.shrc

[[ -f $HOME/work.sh ]] && source $HOME/work.sh

[ -d ~/.zsh/z ] && . ~/.zsh/z/zsh-z.plugin.zsh

# for emacs vterm.el
HOSTNAME=$(uname -n)
USER=$(whoami)

case $TERM in
    xterm*)
        precmd () {print -Pn "\e]0;%n@%m: %~\a"}
        ;;
esac

export GOENV_ROOT=$HOME/.goenv
export PATH="$GOENV_ROOT/bin:$PATH"

if type goenv &> /dev/null; then
  eval "$(goenv init -)"
fi

export PATH="$GOPATH/bin:$PATH"
export GO111MODULE=on
