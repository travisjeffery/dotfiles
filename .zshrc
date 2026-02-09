skip_global_compinit=true

DEFAULT_USERNAME='tj'

# Personal / machine-local setup (interactive-only).
[[ -r "$HOME/work.sh" ]] && source "$HOME/work.sh"

if (( $+commands[wl-copy] )); then
  alias pbcopy='wl-copy'
  alias pbpaste='wl-paste'
fi

export GOENV_ROOT="$HOME/.goenv"
export PATH="$GOENV_ROOT/bin:$PATH"
if type goenv &> /dev/null; then
  eval "$(goenv init -)"
  export PATH="$GOROOT/bin:$PATH"
  export PATH="$PATH:$GOPATH/bin"
fi

export PYENV_ROOT="$HOME/.pyenv"
[[ -d "$PYENV_ROOT/bin" ]] && export PATH="$PYENV_ROOT/bin:$PATH"
if type pyenv &> /dev/null; then
  eval "$(pyenv init - --no-rehash)"
  eval "$(pyenv virtualenv-init -)"
fi

fpath=("$HOME/.zsh/functions" "$HOME/.zsh/completions" "/usr/local/share/zsh/functions" "/usr/local/share/zsh/site-functions" "$HOME/.zsh/zsh-completions" $fpath)

autoload -U compinit && compinit

# fzf-tab (fzf-powered completion UI)
if [[ -r "$HOME/.zsh/fzf-tab/fzf-tab.plugin.zsh" ]] && [[ "${TJ_DISABLE_FZF_TAB:-0}" != 1 ]]; then
  source "$HOME/.zsh/fzf-tab/fzf-tab.plugin.zsh"
  # Show a clear marker when multi-selecting completion candidates.
  # (fzf requires marker display width <= 2; e.g. '[x]' fails on fzf 0.67+.)
  zstyle ':fzf-tab:*' fzf-flags --pointer='>' --marker='*'
  # Make Ctrl-Space toggle selection and move down (overrides the default ctrl-space binding).
  zstyle ':fzf-tab:*' fzf-bindings 'ctrl-space:toggle+down'
fi

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

export FZF_DEFAULT_OPTS="--tiebreak=length,begin --algo=v2 --exact --color=hl:yellow,hl+:yellow:bold"

_cache_completion_script() {
  local cmd="$1"
  local out="$2"
  local exe="${commands[$cmd]}"
  [[ -n "$exe" ]] || return 1

  mkdir -p "${out:h}" 2>/dev/null

  # Refresh when missing or when the executable is newer than the cache.
  if [[ ! -f "$out" || "$exe" -nt "$out" ]]; then
    local tmp="${out}.$$"
    case "$cmd" in
      kubectl) command kubectl completion zsh >| "$tmp" 2>/dev/null ;;
      helm) command helm completion zsh >| "$tmp" 2>/dev/null ;;
      *) return 1 ;;
    esac

    if [[ -s "$tmp" ]]; then
      mv -f "$tmp" "$out" 2>/dev/null || { rm -f "$tmp"; return 1; }
    else
      rm -f "$tmp"
      return 1
    fi
  fi
  [[ -r "$out" ]] || return 1
  source "$out"
}

if (( $+commands[kubectl] )); then
  _cache_completion_script kubectl "${XDG_CACHE_HOME:-$HOME/.cache}/zsh/completions/kubectl.zsh"
fi

if type kubebuilder &> /dev/null; then
  source <(kubebuilder completion zsh)
fi

if (( $+commands[helm] )); then
  _cache_completion_script helm "${XDG_CACHE_HOME:-$HOME/.cache}/zsh/completions/helm.zsh"
fi

source ~/.zsh/completions/_docker

# fzf integration
if (( $+commands[fzf] )); then
  source <(fzf --zsh)

  # `fzf --zsh` binds TAB (^I) to `fzf-completion` (for the `**<TAB>` trigger),
  # which breaks normal path completion like `dir/<TAB>`. Restore standard
  # completion (or fzf-tab's UI if enabled).
  if (( $+widgets[fzf-tab-complete] )); then
    bindkey -M emacs '^I' fzf-tab-complete
  else
    bindkey -M emacs '^I' expand-or-complete
  fi

  bindkey -r '^r'  # unbind Ctrl-R
  bindkey '^[r' fzf-history-widget  # Alt-R for fzf history
fi


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

PROMPT='$ '

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

export HISTFILE="$HOME/.zsh_history"
export HISTSIZE="9999"
export SAVEHIST="9999"
setopt HIST_IGNORE_DUPS
setopt EXTENDED_HISTORY
setopt INC_APPEND_HISTORY

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

function install-all() {
  paru -Sy pyenv \
       libgccjit \
       zoom \
       pyenv-virtualenv \
       docker \
       kubectl \
       magewell-pro-capture-dkms \
       docker-buildx \
       google-chrome \
       sqlite \
       aws-cli \
       aws-cli-v2 \
       helm \
       xkeysnail \
       pulumi \
       chromium \
       wl-clipboard \
       jq \
       krecorder \
       google-chrome
}

function install-emacs (){
  [[ -d ~/code/emacs ]] || git clone git://git.savannah.gnu.org/emacs.git ~/code/emacs
  cd ~/code/emacs && \
    git pull && \
    ./autogen.sh && \
    ./configure \
      --enable-link-time-optimization \
      --with-cairo \
      --with-gnutls \
      --with-harfbuzz \
      --with-json \
      --with-modules \
      --with-native-compilation \
      --with-x-toolkit=no \
      --with-pgtk \
      --with-small-ja-dic \
      --with-threads \
      --with-xml2 \
      --with-zlib && \
    make -j8 && \
    make install
}

if [ $TILIX_ID ] || [ $VTE_VERSION ]; then
  [ -f "/etc/profile.d/vte.sh" ] && source /etc/profile.d/vte.sh
fi

# Read eshell history too
if [[ -f ~/.eshell_history ]]; then
  fc -R ~/.eshell_history
fi

if [[ "$INSIDE_EMACS" = 'vterm' ]]; then
  alias clear='vterm_printf "51;Evterm-clear-scrollback";tput clear'
fi

if (( $+commands[zoxide] )) && [[ "${TJ_DISABLE_ZOXIDE:-0}" != 1 ]]; then
  eval "$(zoxide init zsh)"
fi

case $TERM in
  xterm*)
    precmd () {print -Pn "\e]0;%n@%m: %~\a"}
    ;;
esac

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

vterm_prompt_end() {
  vterm_printf "51;A$(whoami)@$(hostname):$(pwd)";
}
if [[ "$INSIDE_EMACS" == "vterm" ]]; then
  setopt PROMPT_SUBST
  PROMPT=$PROMPT'%{$(vterm_prompt_end)%}'
fi

if [[ -n "$EAT_SHELL_INTEGRATION_DIR" && -n "$INSIDE_EMACS" ]]; then
  source "$EAT_SHELL_INTEGRATION_DIR/zsh"
fi

if [[ -n $GHOSTTY_RESOURCES_DIR ]]; then
  source "$GHOSTTY_RESOURCES_DIR"/shell-integration/zsh/ghostty-integration
fi

if [[ $TERM == "dumb" ]]; then
  unsetopt zle
  PS1='$ '
fi
