source $HOME/.sh_common_login

. .zsh/pure/prompt.zsh

skip_global_compinit=true

autoload -U compinit && compinit
autoload history-search-end
zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end history-search-end
bindkey "^[[A" history-beginning-search-backward-end
bindkey "^[[B" history-beginning-search-forward-end


# bindkey "^P" history-beginning-search-backward-end
# bindkey "^N" history-beginning-search-forward-end

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

# emacs style
bindkey '^a' beginning-of-line
bindkey '^e' end-of-line

bindkey ' ' magic-space                             # [Space] - do history expansion

bindkey '^[[1;5C' forward-word                      # [Ctrl-RightArrow] - move forward one word
bindkey '^[[1;5D' backward-word                     # [Ctrl-LeftArrow] - move backward one word

# Make the delete key (or Fn + Delete on the Mac) work instead of outputting a ~
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

fpath=("$HOME/.zsh/completions" "/usr/local/share/zsh/functions" "/usr/local/share/zsh/site-functions" "$HOME/.zsh/zsh-completions" $fpath)

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

setopt EXTENDED_GLOB AUTO_PUSHD LISTPACKED \
       AUTOREMOVESLASH HIST_IGNORE_ALL_DUPS HIST_IGNORE_DUPS \
       SHARE_HISTORY APPEND_HISTORY
setopt NO_BEEP

setopt correct
setopt interactivecomments
setopt longlistjobs
setopt nobeep
setopt noclobber
setopt notify

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

DEFAULT_USERNAME='tj'

# Threshold (sec) for showing cmd exec time
CMD_MAX_EXEC_TIME=5

# Fastest possible way to check if repo is dirty
git_dirty() {
    git diff --quiet --ignore-submodules HEAD 2>/dev/null; [ $? -eq 1 ] && echo '*'
}

HISTFILE="$HOME/.zsh_history"
HISTSIZE="100"
SAVEHIST="100"

NULL="/dev/null"

bindkey -e

if [ -x "`whence vim`" ]; then
    export EDITOR="`whence vim`"
    alias vi="`whence vim`"
else
    export EDITOR="`whence vi`"
fi

if [ -x "`whence lv`" ]; then
    export PAGER="`whence lv`"
    export LV="-c"
elif [ -x "`whence less`" ]; then
    export PAGER="`whence less`"
    export LESS="-isR"
    alias lv="less"
else
    export PAGER="/bin/more"
fi

# export GREP_OPTIONS='-rIPs --exclude-dir=.[a-zA-Z0-9]* --exclude=.* --exclude=*~ --color=auto'
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
# [[ -x "`whence ctags`" ]] && alias ctags="ctags --sort=foldcase"

alias o=open
alias rake="noglob rake"
# alias irb="pry"
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
alias emacs="emacs -nw"
# alias yaourt="yaourt --tmp /home/tmp"
alias display="display -geometry +0+0"
alias rhino="rlwrap java -jar /usr/share/java/js.jar"

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

source $HOME/.zsh/zaw/zaw.zsh
bindkey '^R' zaw-history
zstyle ':filter-select:highlight' matched fg=yellow,standout
zstyle ':filter-select' max-lines 10
zstyle ':filter-select' max-lines -10
zstyle ':filter-select' case-insensitive yes
zstyle ':filter-select' extended-search yes

source $HOME/.zsh/z/z.sh

if [ -f "$HOME/.dir_colors" ] && [ "${OSTYPE%%[^a-z]*}" != 'darwin' ]; then
  eval `dircolors $HOME/.dir_colors`
fi


BUNDLED_COMMANDS=(cap
capify
cucumber
foreman
guard
haml
heroku
html2haml
jekyll
#pry
rackup
rails
rake
rake2thor
puma
rspec
sass
sass-convert
serve
shotgun
spec
spork
thin
thor
tilt
tt
unicorn
unicorn_rails)

is-bundler-installed()
{
  which bundle > /dev/null 2>&1
}

is-within-bundled-project()
{
  local dir="$(pwd)"
  while [ "$(dirname $dir)" != "/" ]; do
    [ -f "$dir/Gemfile" ] && return
    dir="$(dirname $dir)"
  done
  false
}

run-with-bundler()
{
  if is-bundler-installed && is-within-bundled-project; then
    bundle exec $@
  else
    $@
  fi
}

for CMD in $BUNDLED_COMMANDS; do
  alias $CMD="run-with-bundler $CMD"
done

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
    # zle-line-init () {auto-fu-init;}; zle -N zle-line-init
    zstyle ':completion:*' completer _oldlist _complete
    zle -N zle-keymap-select auto-fu-zle-keymap-select

    zstyle ':auto-fu:highlight' input bold
    zstyle ':auto-fu:highlight' completion fg=white,dim
    zstyle ':auto-fu:highlight' completion/one fg=blue,dim
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

    # delete unambiguous prefix when accepting line
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

if [ -x "brew" ]; then
  if [ -f `brew --prefix`/etc/autojump ]; then
    . `brew --prefix`/etc/autojump
  fi
fi

[ -s "$HOME/.scm_breeze/scm_breeze.sh" ] && source "$HOME/.scm_breeze/scm_breeze.sh"
[ -s "$HOME/.zsh/syntax-highlighting/zsh-syntax-highlighting.zsh" ] && source "$HOME/.zsh/syntax-highlighting/zsh-syntax-highlighting.zsh"


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

### Added by the Heroku Toolbelt
export PATH="/usr/local/heroku/bin:$PATH"

PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting

eval "$(rbenv init - --no-rehash)"
eval "$(rbenv init - --no-rehash)"
eval "$(rbenv init - --no-rehash)"

