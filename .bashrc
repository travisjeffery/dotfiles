# ~/.bashrc (generated from ~/.zshrc) - 2026-01-29 15:19:50
# Backup saved alongside as ~/.bashrc.backup-<timestamp>

# If not running interactively, don't do anything
case $- in
  *i*) ;;
  *) return;;
esac

shopt -s histappend
HISTCONTROL=ignoreboth:erasedups
HISTSIZE=10000
HISTFILESIZE=20000
PROMPT_COMMAND='history -a; history -c; history -r'

# Enable bash completion if available
if [ -r /usr/share/bash-completion/bash_completion ]; then
  . /usr/share/bash-completion/bash_completion
elif [ -r /etc/bash_completion ]; then
  . /etc/bash_completion
fi

# >>> migrated from ~/.zshrc >>>
export FZF_DEFAULT_OPTS="--tiebreak=length,begin --algo=v2 --exact --color=hl:yellow,hl+:yellow:bold"
  source <(kubectl completion bash)
  source <(helm completion bash)
source <(fzf --bash)
export HISTFILE="$HOME/.zsh_history"
export HISTSIZE="9999"
export SAVEHIST="9999"
  export PAGER="`command -v lv`"
  export LV="-c"
  export PAGER="`command -v less`"
  export LESS="-isR"
  alias lv="less"
alias e='emacsclient'
alias pr='git pull-request'
alias rg='rg --ignore-case --hidden'
    alias ls="/bin/ls -A --color=auto"
    export LS_COLORS='no=00:fi=00:di=01;36:ln=00;35:pi=40;33:so=01;35:do=01;35:bd=40;33;01:cd=40;33;01:or=01;05;37;41:mi=01;05;37;41:su=37;41:sg=30;43:tw=30;42:ow=34;42:st=37;44:ex=01;32:'
    alias ls="/bin/ls -AGw"
    alias fetch="fetch -r"
    alias ls="/bin/ls -A"
alias o=open
alias rake="noglob rake"
alias pryr="pry -r ./config/environment -r rails/console/app -r rails/console/helpers"
alias railsc="pryrc"
alias c="clear"
alias l="ls -la"
alias gg="git g"
alias ll="ls -l"
alias lz="ll -Z"
alias df="df -h"
alias du="du -h"
alias gprof="gprof -b"
alias cdrecord="cdrecord driveropts=burnfree"
alias wodim="wodim driveropts=burnfree"
alias display="display -geometry +0+0"
alias rhino="rlwrap java -jar /usr/share/java/js.jar"
alias curl="noglob curl"
alias npm='npm --no-progress'
alias dmc="wrap-docker-machine"
alias dm="docker-machine"
alias d=docker
alias k=kubectl
alias dc="docker-compose"
alias trash="rmtrash"
alias magit='emacsclient -n -e \(magit-status\)'
alias wget='noglob wget'
  alias clear='vterm_printf "51;Evterm-clear-scrollback";tput clear'
# <<< migrated from ~/.zshrc <<<

# >>> fzf history (Alt-r) >>>
if command -v fzf >/dev/null 2>&1; then
  __fzf_history_alt_r() {
    local selected
    selected="$(HISTTIMEFORMAT= builtin history | sed -E 's/^[[:space:]]*[0-9]+[[:space:]]*//' | fzf --tac --no-sort --query "${READLINE_LINE}")" || return
    READLINE_LINE="$selected"
    READLINE_POINT=${#READLINE_LINE}
  }
  bind -x '"\er":__fzf_history_alt_r'
fi
# <<< fzf history (Alt-r) <<<

# Load local overrides (not committed anywhere)
if [ -r "$HOME/.bashrc.local" ]; then
  . "$HOME/.bashrc.local"
fi

