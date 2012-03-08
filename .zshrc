source $HOME/.sh_common_login

autoload -U compinit
autoload history-search-end
zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end history-search-end
bindkey "^[[A" history-beginning-search-backward-end
bindkey "^[[B" history-beginning-search-forward-end
bindkey "^P" history-beginning-search-backward-end
bindkey "^N" history-beginning-search-forward-end

fpath=("$HOME/.zsh/completions" $fpath)

compinit

zstyle ':completion::complete:*' use-cache 1

setopt EXTENDED_GLOB AUTO_PUSHD LISTPACKED \
       AUTOREMOVESLASH HIST_IGNORE_ALL_DUPS HIST_IGNORE_DUPS \
       SHARE_HISTORY APPEND_HISTORY 
setopt NO_BEEP

setopt promptsubst

autoload -U promptinit
promptinit

prompt wunjo

HISTFILE="$HOME/.zsh_history"
HISTSIZE="10000"
SAVEHIST="10000"

NULL="/dev/null"

bindkey -e

#[[ -e "/etc/zsh/zprofile" ]] && source /etc/zsh/zprofile

#
# Set environment variables

#if [ -x "`whence llvm-gcc`" ]; then
#    export CC="llvm-gcc"
#    export CXX="llvm-g++"
#fi

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

# if [ -x "`whence mvn`" ]; then
    # alias mvn="`whence mvn` -DarchetypeGroupId=org.naniyueni -DarchetypeArtifactId=template -DarchetypeVersion=1.0 -DgroupId=org.naniyueni"
# fi


[[ -x "`whence gmcs`" ]] && alias gmcs="gmcs -out:a.out" mcs=gmcs
[[ -x "`whence powerpill`" ]] && alias pacman="`whence powerpill` --nomessages"
[[ -x "`whence rascut`" ]] && alias rascut="_JAVA_OPTIONS=-Duser.language=en `whence rascut`"
[[ -x "`whence mplayer`" ]] && alias mplayer="`whence mplayer` -softvol"
# [[ -x "`whence ctags`" ]] && alias ctags="ctags --sort=foldcase"

alias rake="noglob rake"
alias irb="pry"
alias pryr="pry -r ./config/environment -r rails/console/app -r rails/console/helpers"
alias railsc="pryrc"
alias g="git"
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

PROG="`whence virtualenv`"
[ -x "$PROG" ] && alias virtualenv="$PROG --no-site-packages"

SCREEN_PROG="`whence screen`"
[ -x "$SCREEN_PROG" ] && [ -n "$STY" ] && alias exit="$SCREEN_PROG -d $STY"

TMUX_PROG="`whence tmux`"
[ -x "$TMUX_PROG" ] && [ -n "$TMUX" ] && alias exit="$TMUX_PROG detach"

#
# Set prompt
#
# PROMPT="[%~]
# $ "

# ulimit -c unlimited
umask 072

# if [ "$PS1" -a `uname -s` = "Linux" ]; then
#     mkdir -p -m 0700 /dev/cgroup/cpu/user/$$ > /dev/null 2>&1
#     echo $$ > /dev/cgroup/cpu/user/$$/tasks
#     echo 1 > /dev/cgroup/cpu/user/$$/notify_on_release
# fi

source $HOME/.zsh/zaw/zaw.zsh
bindkey '^R' zaw-history
zstyle ':filter-select:highlight' matched fg=yellow,standout
zstyle ':filter-select' max-lines 10
zstyle ':filter-select' max-lines -10
zstyle ':filter-select' case-insensitive yes
zstyle ':filter-select' extended-search yes

if is-at-least 4.3.10; then
    function () { # precompile
        local A
        A=~/.zsh/auto-fu.zsh/auto-fu.zsh
        [[ -e "${A:r}.zwc" ]] && [[ "$A" -ot "${A:r}.zwc" ]] ||
        zsh -c "source $A; auto-fu-zcompile $A ${A:h}" >/dev/null 2>&1
    }

    source $HOME/.zsh/auto-fu.zsh/auto-fu.zsh
    zle-line-init () {auto-fu-init;}; zle -N zle-line-init
    zstyle ':completion:*' completer _oldlist _complete
    zle -N zle-keymap-select auto-fu-zle-keymap-select

    zstyle ':auto-fu:highlight' input bold
    zstyle ':auto-fu:highlight' completion fg=white,dim
    zstyle ':auto-fu:highlight' completion/one fg=blue,dim
    zstyle ':auto-fu:var' postdisplay ''
    zstyle ':auto-fu:var' track-keymap-skip opp
    zstyle ':auto-fu:var' autoable-function/skiplbuffers \
      'rake *' 'gem *' 'git log *'

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
fi


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
rspec
ruby
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

# if [ "${OSTYPE%%[^a-z]*}" != 'darwin' ]; then
  for CMD in $BUNDLED_COMMANDS; do
    alias $CMD="run-with-bundler $CMD"
  done
# fi
