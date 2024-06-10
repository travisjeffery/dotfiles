skip_global_compinit=true

DEFAULT_USERNAME='tj'

autoload -U compinit && compinit

autoload history-search-end
autoload -U url-quote-magic

zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end history-search-end
zle -N self-insert url-quote-magic

bindkey "^[[A" history-beginning-search-backward-end
bindkey "^[[B" history-beginning-search-forward-end
bindkey '\ew' kill-region                           # [Esc-w] - Kill from the cursor to the mark
bindkey -s '\el' 'ls\n'                             # [Esc-l] - run command: ls
bindkey -s '\e.' '..\n'                             # [Esc-.] - run command: .. (up directory)
bindkey '^r' history-incremental-search-backward    # [Ctrl-r] - Search backward incrementally for a specified string. The string may begin with ^ to anchor the search to the beginning of the line.
bindkey '^[[5~' up-line-or-history                  # [PageUp] - Up a line of history
bindkey '^[[6~' down-line-or-history                # [PageDown] - Down a line of history
bindkey "\e[Z" reverse-menu-complete # Shift+Tab
bindkey "^P" reverse-menu-complete
bindkey "^N" menu-complete
bindkey '^[[A' up-line-or-search                    # start typing + [Up-Arrow] - fuzzy find history forward
bindkey '^[[B' down-line-or-search                  # start typing + [Down-Arrow] - fuzzy find history backward
bindkey '^[[H' beginning-of-line                    # [Home] - Go to beginning of line
bindkey '^[[1~' beginning-of-line                   # [Home] - Go to beginning of line
bindkey '^[OH' beginning-of-line                    # [Home] - Go to beginning of line
bindkey '^[[F'  end-of-line                         # [End] - Go to end of line
bindkey '^[[4~' end-of-line                         # [End] - Go to end of line
bindkey '^[OF' end-of-line                          # [End] - Go to end of line
bindkey '^a' beginning-of-line
bindkey '^e' end-of-line
bindkey ' ' magic-space                             # [Space] - do history expansion
bindkey '^[[1;5C' forward-word                      # [Ctrl-RightArrow] - move forward one word
bindkey '^[[1;5D' backward-word                     # [Ctrl-LeftArrow] - move backward one word
bindkey '^?' backward-delete-char                   # [Delete] - delete backward
bindkey '^[[3~' delete-char                         # [fn-Delete] - delete forward
bindkey '^[3;5~' delete-char
bindkey '\e[3~' delete-char

zmodload -i zsh/parameter
insert-last-command-output() {
  LBUFFER+="$(eval $history[$((HISTCMD-1))])"
}
zle -N insert-last-command-output
bindkey "^X^L" insert-last-command-output

fpath=("$HOME/.zsh/functions" "$HOME/.zsh/completions" "/usr/local/share/zsh/functions" "/usr/local/share/zsh/site-functions" "$HOME/.zsh/zsh-completions" $fpath)

zstyle ':completion::complete:*' use-cache 1
zstyle ':completion:*'          list-colors ''
zstyle ':completion:*'          insert-tab pending
zstyle ':completion:*'          matcher-list 'm:{[:lower:]}={[:upper:]}'
zstyle ':completion:*'          special-dirs true
zstyle ':completion:*:cd:*'     ignore-parents parent pwd
zstyle ':completion:*:warnings' format "zsh: no matches found."

# Fuzzy matching of completions for when you mistype them
zstyle ':completion:*' completer _complete _match _approximate
zstyle ':completion:*:match:*' original only
zstyle ':completion:*:approximate:*' max-errors 1 numeric

source $HOME/.zsh/zaw/zaw.zsh
bindkey '^R' zaw-history
zstyle ':filter-select:highlight' matched fg=yellow,standout
zstyle ':filter-select' max-lines 10
zstyle ':filter-select' max-lines -10
zstyle ':filter-select' case-insensitive yes
zstyle ':filter-select' extended-search yes


backward-delete-to-slash() {
  integer pos=$CURSOR
  while (( pos > 1 )); do
    if [[ $LBUFFER[--pos] = / ]]; then
      LBUFFER=${LBUFFER[1,pos]}
      return 0
    fi
  done
  return 1
}

zle -N backward-delete-to-slash

zstyle ':completion:*:*:git:*' user-commands author:'show author info'

export FZF_DEFAULT_OPTS="--tiebreak=length,begin --algo=v2 --exact"

if type kubectl &> /dev/null; then
  source <(kubectl completion zsh)
fi

if type kubebuilder &> /dev/null; then
  source <(kubebuilder completion zsh)
fi

if type helm &> /dev/null; then
  source <(helm completion zsh)
fi

source ~/.zsh/completions/_docker

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh


tj-backward-kill() {
  local WORDCHARS='*?_~=&;!#$%^(){}'
  zle backward-kill-word;
};
zle -N tj-backward-kill;
bindkey '^w' tj-backward-kill;

tj-backward-kill-word () {
  local WORDCHARS=${WORDCHARS/\/}
  zle backward-kill-word
}
zle -N tj-backward-kill-word
bindkey '^[^?' tj-backward-kill-word

tj-backward-word() {
  local WORDCHARS='*?_~=&;!#$%^(){}'
  zle backward-word;
};
zle -N tj-backward-word;

tj-forward-word() {
  local WORDCHARS='*?_~=&;!#$%^(){}'
  zle forward-word;
};
zle -N tj-forward-word;

autoload smart-insert-last-word
zle -N insert-last-word smart-insert-last-word
zstyle :insert-last-word match \
       '*([^[:space:]][:alpha:]/\\]|[[:alpha:]/\\][^[:space:]])*'
bindkey '^]' insert-last-word

autoload -U modify-current-argument
_quote-previous-word-in-single() {
  modify-current-argument '${(qq)${(Q)ARG}}'
  zle vi-forward-blank-word
}
zle -N _quote-previous-word-in-single

_quote-previous-word-in-double() {
  modify-current-argument '${(qqq)${(Q)ARG}}'
  zle vi-forward-blank-word
}
zle -N _quote-previous-word-in-double

function () { # precompile
  local A
  A=~/.zsh/auto-fu.zsh/auto-fu.zsh
  [[ -e "${A:r}.zwc" ]] && [[ "$A" -ot "${A:r}.zwc" ]] ||
    zsh -c "source $A; auto-fu-zcompile $A ${A:h}" >/dev/null 2>&1
}

source $HOME/.zsh/auto-fu.zsh/auto-fu.zsh
zstyle ':completion:*' completer _oldlist _complete
zle -N zle-keymap-select auto-fu-zle-keymap-select

zstyle ':auto-fu:highlight' input bold
zstyle ':auto-fu:highlight' completion fg=white,dim
zstyle ':auto-fu:highlight' completion/one fg=blue,dim
# zstyle ':auto-fu:var' enable all
zstyle ':auto-fu:var' postdisplay ''
zstyle ':auto-fu:var' track-keymap-skip opp
zstyle ':auto-fu:var' autoable-function/skiplbuffers \
       'rake *' 'gem *' 'git log *' 'npm*'

function afu+cancel () {
  afu-clearing-maybe
  ((afu_in_p == 1)) && { afu_in_p=0; BUFFER="$buffer_cur"; }
}

zle -N afu+cancel

function bindkey-advice-before () {
  local key="$1"
  local advice="$2"
  local widget="$3"
  [[ -z "$widget" ]] && {
    local -a bind
    bind=(`bindkey -M main "$key"`)
    widget=$bind[2]
  }
  local fun="$advice"
  if [[ "$widget" != "undefined-key" ]]; then
    local code=${"$(<=(cat <<"EOT"
            function $advice-$widget () {
                zle $advice
                zle $widget
            }
            fun="$advice-$widget"
EOT
        ))"}
    eval "${${${code//\$widget/$widget}//\$key/$key}//\$advice/$advice}"
  fi
  zle -N "$fun"
  bindkey -M afu "$key" "$fun"
}

bindkey-advice-before "^G" afu+cancel
bindkey-advice-before "^[" afu+cancel
bindkey-advice-before "^J" afu+cancel afu+accept-line

function afu+delete-unambiguous-prefix () {
  afu-clearing-maybe
  local buf; buf="$BUFFER"
  local bufc; bufc="$buffer_cur"
  [[ -z "$cursor_new" ]] && cursor_new=-1
  [[ "$buf[$cursor_new]" == ' ' ]] && return
  [[ "$buf[$cursor_new]" == '/' ]] && return
  ((afu_in_p == 1)) && [[ "$buf" != "$bufc" ]] && {
    # there are more than one completion candidates
    zle afu+complete-word
    [[ "$buf" == "$BUFFER" ]] && {
      # the completion suffix was an unambiguous prefix
      afu_in_p=0; buf="$bufc"
    }
    BUFFER="$buf"
    buffer_cur="$bufc"
  }
}

zle -N afu+delete-unambiguous-prefix
function afu-ad-delete-unambiguous-prefix () {
  local afufun="$1"
  local code; code=$functions[$afufun]
  eval "function $afufun () { zle afu+delete-unambiguous-prefix; $code }"
}

afu-ad-delete-unambiguous-prefix afu+accept-line
afu-ad-delete-unambiguous-prefix afu+accept-line-and-down-history
afu-ad-delete-unambiguous-prefix afu+accept-and-hold

PROMPT='; '

setopt EXTENDED_GLOB AUTO_PUSHD LISTPACKED \
       AUTOREMOVESLASH HIST_IGNORE_ALL_DUPS HIST_IGNORE_DUPS \
       SHARE_HISTORY APPEND_HISTORY
setopt NO_BEEP
setopt globdots

setopt correct
setopt interactivecomments
setopt longlistjobs
setopt nobeep
setopt noclobber
setopt AUTO_CD

setopt notify
setopt AUTO_NAME_DIRS
setopt PUSHD_MINUS

setopt no_bare_glob_qual

setopt appendhistory
setopt extendedhistory
setopt histexpiredupsfirst
setopt histignoredups
setopt histreduceblanks
setopt histverify
setopt incappendhistory

setopt autopushd
setopt pushdignoredups

setopt combiningchars
setopt noautomenu

autoload -U select-word-style
select-word-style bash

# Threshold (sec) for showing cmd exec time
CMD_MAX_EXEC_TIME=5

# Fastest possible way to check if repo is dirty
git_dirty() {
  git diff --quiet --ignore-submodules HEAD 2>/dev/null; [ $? -eq 1 ] && echo '*'
}

HISTFILE="$HOME/.zsh_history"
HISTSIZE="9999"
SAVEHIST="9999"

NULL="/dev/null"

bindkey -e

if [ -x "`whence lv`" ]; then
  export PAGER="`whence lv`"
  export LV="-c"
elif [ -x "`whence less`" ]; then
  export PAGER="`whence less`"
  export LESS="-isR"
  alias lv="less"
else
  # export PAGER="/bin/more"
fi

alias e='emacsclient'
alias pr='git pull-request'
alias rg='rg --ignore-case --hidden'

#
# Set aliases
#

setopt complete_aliases

case "$OSTYPE" in
  linux*)
    alias ls="/bin/ls -A --color=auto"
    export LS_COLORS='no=00:fi=00:di=01;36:ln=00;35:pi=40;33:so=01;35:do=01;35:bd=40;33;01:cd=40;33;01:or=01;05;37;41:mi=01;05;37;41:su=37;41:sg=30;43:tw=30;42:ow=34;42:st=37;44:ex=01;32:'
    zstyle ':completion:*' list-colors 'di=01;36' 'ln=00;35' 'so=01;35' 'ex=01;32' 'bd=40;33;01' 'cd=40;33;01'
    alias -g pg="-pg -g -static -lc_p"
    ;;
  freebsd*)
    alias ls="/bin/ls -AGw"
    alias fetch="fetch -r"
    ;;
  *)
    alias ls="/bin/ls -A"
    ;;
esac

[[ -x "`whence gmcs`" ]] && alias gmcs="gmcs -out:a.out" mcs=gmcs
[[ -x "`whence powerpill`" ]] && alias pacman="`whence powerpill` --nomessages"
[[ -x "`whence rascut`" ]] && alias rascut="_JAVA_OPTIONS=-Duser.language=en `whence rascut`"
[[ -x "`whence mplayer`" ]] && alias mplayer="`whence mplayer` -softvol"

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
alias wget="noglob wget
"
function g {
  if [[ $# > 0 ]]; then
    git $@
  else
    git status
  fi
}

compdef g=git

PROG="`whence virtualenv`"
[ -x "$PROG" ] && alias virtualenv="$PROG --no-site-packages"

SCREEN_PROG="`whence screen`"
[ -x "$SCREEN_PROG" ] && [ -n "$STY" ] && alias exit="$SCREEN_PROG -d $STY"

TMUX_PROG="`whence tmux`"
[ -x "$TMUX_PROG" ] && [ -n "$TMUX" ] && alias exit="$TMUX_PROG detach"

# ulimit -c unlimited
umask 072


if [ -f "$HOME/.dir_colors" ] && [ "${OSTYPE%%[^a-z]*}" != 'darwin' ]; then
  eval `dircolors $HOME/.dir_colors`
fi

dev () {
  local dev="$HOME/dev/travisjeffery"
  local dir="$dev/$@"

  if [ -d "$dir" ]; then
    cd "$dir"
  else
    cd "$dev"
    git clone "git@github.com:travisjeffery/$@.git" &> /dev/null || { read "?clone url: " url && git clone "$url" }
    cd "$@"
  fi
}

function ec2-ip () {
  aws ec2 describe-instances --filter Name=instance-id,Values=$1 | jq '.Reservations[0].Instances[0].PrivateIpAddress' | tr -d '"'
}

function ec2-ssh () {
  ssh $(ec2-ip $1)
}


alias npm='npm --no-progress'

wrap-docker-machine() {
  config="$1"
  shift
  docker $(docker-machine config "$config") $@
}
alias dmc="wrap-docker-machine"
alias dm="docker-machine"
alias d=docker
alias k=kubectl
alias dc="docker-compose"
alias trash="rmtrash"
alias magit='emacsclient -n -e \(magit-status\)'
alias wget='noglob wget'

function git-ignore() {
  local lang=$1
  curl https://raw.githubusercontent.com/github/gitignore/master/$lang.gitignore > .gitignore
}

function install-emacs (){
  [[ -d ~/code/emacs ]] || git clone git://git.savannah.gnu.org/emacs.git ~/code/emacs
  cd ~/code/emacs && \
    git pull && \
    ./autogen.sh && \
    ./configure --with-json --with-cairo --with-rsvg --with-native-compilation --with-modules --with-mailutils --with-pgtk && \
    make -j8 && \
    make install
}

if [ $TILIX_ID ] || [ $VTE_VERSION ]; then
  [ -f "/etc/profile.d/vte.sh" ] && source /etc/profile.d/vte.sh
fi

if [[ "$INSIDE_EMACS" = 'vterm' ]]; then
  alias clear='vterm_printf "51;Evterm-clear-scrollback";tput clear'
fi

[ -d ~/.zsh/z ] && . ~/.zsh/z/zsh-z.plugin.zsh

case $TERM in
  xterm*)
    precmd () {print -Pn "\e]0;%n@%m: %~\a"}
    ;;
esac

gam() { "/home/tj/bin/gam/gam" "$@" ; }

vterm_printf() {
  if [ -n "$TMUX" ] && ([ "${TERM%%-*}" = "tmux" ] || [ "${TERM%%-*}" = "screen" ] ); then
    # Tell tmux to pass the escape sequences through
    printf "\ePtmux;\e\e]%s\007\e\\" "$1"
  elif [ "${TERM%%-*}" = "screen" ]; then
    # GNU screen (screen, screen-256color, screen-256color-bce)
    printf "\eP\e]%s\007\e\\" "$1"
  else
    printf "\e]%s\e\\" "$1"
  fi
}

function hostname() {
  cat /etc/hostname
}

vterm_prompt_end() {
  vterm_printf "51;A$(whoami)@$(hostname):$(pwd)";
}
setopt PROMPT_SUBST
PROMPT=$PROMPT'%{$(vterm_prompt_end)%}'

export PYENV_ROOT="$HOME/.pyenv"
[[ -d $PYENV_ROOT/bin ]] && export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init -)"
