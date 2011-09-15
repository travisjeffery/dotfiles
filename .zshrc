# ~/.zshrc

# Section: Environment {{{1
# -------------------------

setopt rmstarsilent histignoredups
setopt noclobber nonomatch
setopt completeinword extendedglob
setopt autocd

if [[ $ZSH_VERSION == 4.<->* ]]; then
    setopt histexpiredupsfirst histreduceblanks
fi

fpath=($fpath ~/.zsh/functions ~/.zsh/functions.zwc ~/.rvm/scripts/zsh/Completion)
# watch=(notme)
# [ -f "$HOME/.friends" ] && watch=(`cat "$HOME/.friends"`)
HISTSIZE=100
WORDCHARS='*?_-.[]~&;!#$%^(){}<>'
PERIOD=3600
periodic() { rehash }

export ENV="$HOME/.shrc"
interactive=1
. "$ENV"

#domains=(`egrep '^(search|domain)' /etc/resolv.conf 2>/dev/null`)
#[[ -z $domains ]] || shift 1 domains

namedir() { export $1=$PWD; : ~$1 }

#for host in $domains; do
#    boxen=(${boxen%.$host})
#    family=(${family%.$host})
#    friends=(${friends%.$host})
#done

unset interactive domains host
# Section: Prompt {{{1
# --------------------
e=`echo -ne "\e"`

git_prompt_info() {
    if [ -d .svn ]; then
        ref=.svn
    else
        ref=${$(git symbolic-ref HEAD 2> /dev/null)#refs/heads/} || \
        ref=${$(git rev-parse HEAD 2>/dev/null)[1][1,7]} || \
        return
    fi
    case "$TERM" in
        screen*) branchcolor=$'\e[38;5;31m'   ;;
        *)       branchcolor="$fg_bold[blue]" ;;
    esac
    case "$ref" in ????????????????????*) ref="${ref[1,17]}..." ;; esac
    echo "(%{$branchcolor%}${ref}%{$reset_color%})"
}

autoload -U colors && colors

hostcolor="01;37"

local usercolor="$fg_bold[yellow]"
local dircolor="$fg_bold[blue]"
case "$TERM" in
    screen*)
    usercolor=$'\e[38;5;184m'
    dircolor=$'\e[38;5;27m'
    ;;
    xterm*|rxvt-unicode)
    usercolor=$'\e[93m'
    dircolor=$'\e[94m'
    ;;
esac
[ $UID = '0' ] && usercolor="$fg_bold[white]"
reset_color=$'\e[00m'

PROMPT="%{$usercolor%}%n%{${e}[00m%}@%{${e}[m%}%m%{${e}[00m%}:%{$dircolor%}%30<...<%~%<<%{${e}[00m%}%{${e}[00m%}\$(git_prompt_info)%# "
RPS1="%(?..(%{${e}[01;35m%}%?%{${e}[00m%}%)%<<)"
setopt promptsubst

case ${OLDTERM:-$TERM} in
screen*|vt220*)
    hostcode="+b W"
    usercode="+b Y"
    [ $UID = '0' ] && usercode="+b W"

    screenhs="\005{$usercode}%n\005{-}@\005{$hostcode}%m\005{-}:\005{+b B}%~\005{-}"
    precmd  () {local tty="`print -P "%l@"|sed -e s,/,-,g`"
                # print -Pn "\e]1;\a\e]1;$tty%m\a"
                print -Pn "\e]2;$screenhs [%l]\a"
                if [ "$STY" ]; then
                    print -Pn "\e]1;\a\e]1;@%m\a"
                    # $tty
                    print -Pn "\ek@\e\\"
                else
                    print -Pn "\ek@%m\e\\"
                fi
            }
    preexec () {local tty="`print -P "%l@"|sed -e s,/,-,g`"
                local cmd="$1"
                case "$cmd" in
                    ???????????*) cmd="${cmd%% *}" ;;
                esac
                case "$cmd" in
                    ???????????*) cmd="${cmd%%/*}" ;;
                esac
                cmd=$(echo -n "$cmd"|tr '\0-\037%$' '.')
                # print -Pn "\e]1;\a\e]1;$tty%m*\a"
                print -Pn "\e]2;$screenhs"
                print -Pnr " (%24>..>$cmd"
                print -Pn ") [%l]\a"
                if [ "$STY" ]; then
                    print -Pn "\ek$cmd@\e\\"
                else
                    print -Pn "\ek$cmd@%m\e\\"
                fi
                }
    #[ "`hostname`" = grex.cyberspace.org ] &&TERM=vt220 &&export OLDTERM=screen
    ;;
xterm*|rxvt*|Eterm*|kterm*|putty*|dtterm*|ansi*|cygwin*)
    precmd  () {local tty="`print -P "%l@"|sed -e s,/,-,g`"
                print -Pn "\e]1;$tty%m\a"
                print -Pn "\e]2;%n@%m:%~ [%l]\a"
                }
    preexec () {local tty="`print -P "%l@"|sed -e s,/,-,g`"
                print -Pn "\e]1;$tty%m*\a"
                print -Pn "\e]2;%n@%m:%~"
                print -Pnr " (%24>..>$1"|tr '\0-\037' '.'
                print -Pn ") [%l]\a"
                } ;;
linux) ;;
*)
    PS1="$hostletter%# "
    RPS1="%(?..(%?%)%<<)"
    ;;
esac

unset hostcolor hostletter hostcode dircolor usercolor usercode
unset e
# Section: Keybindings {{{1
# -------------------------
bindkey -e
#bindkey -m
bindkey "\e[3~" delete-char
bindkey "\e[1~" beginning-of-line
bindkey "\e[7~" beginning-of-line
bindkey "\e[H"  beginning-of-line
bindkey "\eOH"  beginning-of-line
bindkey "\e[4~" end-of-line
bindkey "\e[8~" end-of-line
bindkey "\e[F"  end-of-line
bindkey "\eOF"  end-of-line
bindkey -r "^Q"

bindkey "\eb"   emacs-backward-word
bindkey "\ef"   emacs-forward-word

bindkey -M viins "^A" beginning-of-line
bindkey -M viins "^B" backward-char
bindkey -M viins "^D" delete-char-or-list
bindkey -M viins "^E" end-of-line
bindkey -M viins "^F" forward-char
bindkey -M viins "^K" kill-line
bindkey -M viins "^N" next-history
bindkey -M viins "^P" previous-history

bindkey "^X^[" vi-cmd-mode

case $ZSH_VERSION in
3.*) ;;
*)
beginning-of-sentence() {
    local WORDCHARS="'"'*?_-.[]~=/!#$%^(){}<>"  ' #'
    zle backward-word
}
zle -N beginning-of-sentence
#bindkey "\ea" beginning-of-sentence

end-of-sentence() {
    local WORDCHARS="'"'*?_-.[]~=/!#$%^(){}<>"  ' #'
    zle forward-word
}
zle -N end-of-sentence
#bindkey "\ee" end-of-sentence

change-first-word() {
    zle beginning-of-line -N
    zle kill-word
}
zle -N change-first-word
bindkey "\eE" change-first-word
bindkey "\ea" change-first-word

new-screen() {
    [ -z "$STY" ] || screen < "$TTY"
}
zle -N new-screen
[[ -z "$terminfo[kf12]" ]] || bindkey "$terminfo[kf12]" new-screen
[[ -z "$terminfo[kf11]" ]] || bindkey -s "$terminfo[kf11]" "^Ascreen ^E\n"

zle -N edit-command-line
bindkey '^[e' edit-command-line
bindkey '^X^E' edit-command-line
bindkey -M vicmd v edit-command-line

;;
esac

[[ -z "$terminfo[kich1]" ]] || bindkey -M emacs "$terminfo[kich1]" overwrite-mode
[[ -z "$terminfo[kdch1]" ]] || bindkey -M emacs "$terminfo[kdch1]" delete-char
[[ -z "$terminfo[khome]" ]] || bindkey -M emacs "$terminfo[khome]" beginning-of-line
[[ -z "$terminfo[kend]"  ]] || bindkey -M emacs "$terminfo[kend]" end-of-line
[[ -z "$terminfo[kpp]"   ]] || bindkey -M emacs "$terminfo[kpp]" beginning-of-history
[[ -z "$terminfo[knp]"   ]] || bindkey -M emacs "$terminfo[knp]" end-of-history
#for widget in kill-word backward-kill-word forward-word backward-word up-case-word down-case-word transpose-words; do autoload bash-$widget; zle -N $widget bash-$widget; done

# Section: Aliases {{{1
# ---------------------
alias lsd='ls -ld *(-/DN)' # directories only
alias sb='noglob sensible-browser'
alias zmv='noglob zmv'
alias ru='noglob ru'

which sudo >/dev/null && alias sudo='sudo ' # this makes $1 expand as an alias

# Global aliases -- These do not have to be
# at the beginning of the command line.
# alias -g MM='|more'
# alias -g HH='|head'
# alias -g TT='|tail'
# alias -g LL='|less'

# Section: Modules {{{1
# ---------------------
if [[ $ZSH_VERSION == 3.<->* ]]; then
    which zmodload >&/dev/null && zmodload zsh/compctl
    compctl -c sudo
    compctl -c which
    compctl -g '*(-/)' + -g '.*(-/)' -v cd pushd rmdir
    compctl -k hosts -x 'p[2,-1]' -l '' -- rsh ssh
    return 0
fi

zmodload -i zsh/mathfunc
#zmodload -i zsh/complist
autoload -U zrecompile
autoload -U zmv
autoload -U zsh-mime-setup
autoload -U edit-command-line

# Section: Styles {{{1
# ------------------------
zstyle ':mime:*' x-browsers sensible-browser
zstyle ':mime:*' tty-browsers sensible-browser
zstyle ':mime:*' mailcap ~/.mailcap
# The following lines were added by compinstall

zstyle ':completion:*' add-space true
zstyle -e ':completion:*' completer '
        if [[ $_last_try != "$HISTNO$BUFFER$CURSOR" ]]; then
            _last_try="$HISTNO$BUFFER$CURSOR"
            reply=(_complete _ignored:complete _prefix _complete:full)
        else
            reply=(_complete _ignored:complete _prefix _complete:full _correct _approximate)
        fi' #'
zstyle ':completion::prefix:*' completer _complete _ignored:complete
zstyle ':completion:*' format 'Completing %d'
zstyle ':completion:*' glob 1
zstyle ':completion::complete:*:(all-|)files' ignored-patterns '*\~' '(|*/)CVS'
zstyle ':completion::complete:*:(local-|)directories' ignored-patterns '(|*/)CVS'
zstyle ':completion::complete:*' ignore-parents parent pwd
zstyle ':completion::complete:rm::(all-|)files' ignored-patterns
zstyle ':completion::complete:rmdir::(local-|)directories' ignored-patterns
zstyle ':completion:*' group-name ''
zstyle ':completion:*' hosts localhost $friends
zstyle ':completion:*' urls http://www.google.com/
zstyle ':completion:*' insert-unambiguous true
# NO NO NO!!! This makes things SLOW
#zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' list-colors no=00 fi=00 di=01\;34 pi=33 so=01\;35 bd=00\;35 cd=00\;34 or=00\;41 mi=00\;45 ex=01\;32
zstyle ':completion:*' list-prompt '%SAt %p: Hit TAB for more, or the character to insert%s'
zstyle ':completion:*' local localhost /var/www public_html
zstyle ':completion:*' matcher-list '' 'm:{a-z}={A-Z}'
zstyle ':completion::full:*' matcher-list 'm:{a-zA-Z}={A-Za-z}' '+r:|[._-/]=* r:|=* l:|[._-/]=* l:|=*'
zstyle -e ':completion:*' max-errors 'reply=( $(( ($#PREFIX+$#SUFFIX+1)/3 )) numeric )'
zstyle ':completion:*' menu select
zstyle ':completion:*:(xdvi|xpdf|gv|mpl):*' menu yes select
zstyle ':completion:*' original true
zstyle ':completion:*' select-prompt '%SScrolling active: current selection at %p%s'
zstyle ':completion:*' substitute 1
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path ~/.zsh/cache
# zstyle ':completion:*' users travis root $USER ${watch/notme/}
zstyle ':completion:*' verbose true
zstyle ':completion:*:rm:*' ignore-line yes
zstyle :compinstall filename "$HOME/.zshrc"

autoload -U compinit
compinit -u
# End of lines added by compinstall
compdef 'local expl; _description files expl "LaTeX aux file"; _files "$expl[@]" -g "*.aux"' bibtex
compdef 'local expl; _description files expl "picture file"; _files "$expl[@]" -g "*.(#i)(png|gif|jpeg|jpg|tiff|tif|pbm|pgm|ppm|xbm|xpm|ras(|t)|tga|rle|rgb|bmp|pcx|fits|pm)(-.)"' feh

# Last resort  #{{{1
# RVM: Ruby Version Manager  #{{{2
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm"


