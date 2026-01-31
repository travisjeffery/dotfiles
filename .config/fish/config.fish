# Fish equivalents for settings in `~/.zshrc`.

if not status is-interactive
    return
end

umask 072

function __tj_prepend_path --argument-names dir
    if test -n "$dir"; and test -d "$dir"; and not contains -- "$dir" $PATH
        set -gx PATH "$dir" $PATH
    end
end

function __tj_append_path --argument-names dir
    if test -n "$dir"; and test -d "$dir"; and not contains -- "$dir" $PATH
        set -gx PATH $PATH "$dir"
    end
end

function __tj_prepend_manpath --argument-names dir
    if test -n "$dir"; and test -d "$dir"; and not contains -- "$dir" $MANPATH
        set -gx MANPATH "$dir" $MANPATH
    end
end

function __tj_prepend_infopath --argument-names dir
    if test -n "$dir"; and test -d "$dir"; and not contains -- "$dir" $INFOPATH
        set -gx INFOPATH "$dir" $INFOPATH
    end
end

# Prefer nicer pagers when available.
if type -q lv
    set -gx PAGER (command -v lv)
    set -gx LV -c
else if type -q less
    set -gx PAGER (command -v less)
    set -gx LESS -isR
    alias lv=less
end

set -gx FZF_DEFAULT_OPTS "--tiebreak=length,begin --algo=v2 --exact --color=hl:yellow,hl+:yellow:bold"

# Completions (generated at runtime when tools are installed)
if type -q kubectl
    kubectl completion fish | source
end

if type -q kubebuilder
    kubebuilder completion fish | source
end

if type -q helm
    helm completion fish | source
end

# fzf integration + keybindings
# fzf integration + keybindings
if type -q fzf
    fzf --fish | source
    bind \cr 'commandline -f repaint'
    bind \er fzf-history-widget
end

alias e=emacsclient
alias pr='git pull-request'
alias rg='rg --ignore-case --hidden'

switch (uname)
    case Linux
        alias ls='/bin/ls -A --color=auto'
        set -gx LS_COLORS 'no=00:fi=00:di=01;36:ln=00;35:pi=40;33:so=01;35:do=01;35:bd=40;33;01:cd=40;33;01:or=01;05;37;41:mi=01;05;37;41:su=37;41:sg=30;43:tw=30;42:ow=34;42:st=37;44:ex=01;32:'
    case FreeBSD
        alias ls='/bin/ls -AGw'
        if type -q fetch
            alias fetch='fetch -r'
        end
    case '*'
        alias ls='/bin/ls -A'
end

# If you use `~/.dir_colors`, prefer it over the default `LS_COLORS` above.
function __tj_load_dircolors
    if test (uname) = Darwin
        return 0
    end
    if not type -q dircolors
        return 0
    end
    if not test -f "$HOME/.dir_colors"
        return 0
    end

    set -l dc (dircolors -b "$HOME/.dir_colors" ^/dev/null)
    for line in $dc
        if string match -q -r '^LS_COLORS=' -- $line
            set -l val (string replace -r "^LS_COLORS='([^']*)'.*\$" '$1' -- $line)
            test -n "$val"; and set -gx LS_COLORS $val
            return 0
        end
    end
end
__tj_load_dircolors

if type -q gmcs
    alias gmcs='gmcs -out:a.out'
    alias mcs=gmcs
end

if type -q powerpill
    alias pacman='powerpill --nomessages'
end

if type -q rascut
    function rascut --wraps rascut
        env _JAVA_OPTIONS=-Duser.language=en rascut $argv
    end
end

if type -q mplayer
    alias mplayer='mplayer -softvol'
end

if type -q open
    alias o=open
end

if type -q wl-copy
    alias pbcopy=wl-copy
    alias pbpaste=wl-paste
end

alias c=clear
alias l='ls -la'
alias ll='ls -l'
alias lz='ll -Z'
alias gg='git g'
alias df='df -h'
alias du='du -h'
alias gprof='gprof -b'
alias cdrecord='cdrecord driveropts=burnfree'
alias wodim='wodim driveropts=burnfree'
alias display='display -geometry +0+0'

if type -q rlwrap
    alias rhino='rlwrap java -jar /usr/share/java/js.jar'
end

# fish expands globs in unquoted URLs; prefer quoting URLs instead of zsh's `noglob`.
# (e.g. `curl \"https://example.com?a=b\"`)

function g --wraps git
    if test (count $argv) -gt 0
        command git $argv
    else
        command git status
    end
end
complete -c g -w git

if type -q virtualenv
    alias virtualenv='virtualenv --no-site-packages'
end

if set -q STY; and type -q screen
    function exit --wraps exit
        command screen -d "$STY"
    end
end

if set -q TMUX; and type -q tmux
    function exit --wraps exit
        command tmux detach
    end
end

function dev
    set -l base "$HOME/dev/travisjeffery"

    if test (count $argv) -eq 0
        cd "$base"
        return
    end

    set -l repo $argv[1]
    set -l dir "$base/$repo"

    if test -d "$dir"
        cd "$dir"
        return
    end

    cd "$base"
    command git clone "git@github.com:travisjeffery/$repo.git" >/dev/null ^/dev/null
    or begin
        read -P "clone url: " url
        test -n "$url"; and command git clone "$url"
    end
    cd "$repo"
end

function ec2-ip
    aws ec2 describe-instances --filter "Name=instance-id,Values=$argv[1]" | jq '.Reservations[0].Instances[0].PrivateIpAddress' | tr -d '"'
end

function ec2-ssh
    ssh (ec2-ip $argv[1])
end

alias npm='npm --no-progress'

function wrap-docker-machine
    set -l config $argv[1]
    set -e argv[1]

    if not type -q docker-machine
        echo "wrap-docker-machine: docker-machine not found" >&2
        return 127
    end

    set -l dm_flags (docker-machine config "$config" | string collect)
    set -l escaped_args (string join ' ' -- (string escape -- $argv))
    eval "command docker $dm_flags $escaped_args"
end

alias dmc=wrap-docker-machine
alias dm=docker-machine
alias d=docker
alias k=kubectl
alias dc=docker-compose
alias trash=rmtrash
alias magit="emacsclient -n -e '(magit-status)'"

function git-ignore
    set -l lang $argv[1]
    command curl "https://raw.githubusercontent.com/github/gitignore/master/$lang.gitignore" > .gitignore
end

function install-all
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
end

function install-emacs
    test -d "$HOME/code/emacs"; or command git clone git://git.savannah.gnu.org/emacs.git "$HOME/code/emacs"
    cd "$HOME/code/emacs"; and command git pull; and \
        ./autogen.sh; and \
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
        --with-zlib; and \
        make -j8; and \
        make install
end

if set -q TILIX_ID; or set -q VTE_VERSION
    test -f /etc/profile.d/vte.fish; and source /etc/profile.d/vte.fish
end

if test "$INSIDE_EMACS" = vterm
    function vterm_printf
        set -l term_prefix (string split -m1 '-' -- "$TERM")[1]
        if test -n "$TMUX"; and begin; test "$term_prefix" = tmux; or test "$term_prefix" = screen; end
            printf '\ePtmux;\e\e]%s\007\e\\' "$argv[1]"
        else if test "$term_prefix" = screen
            printf '\eP\e]%s\007\e\\' "$argv[1]"
        else
            printf '\e]%s\e\\' "$argv[1]"
        end
    end

    function vterm_prompt_end
        vterm_printf "51;A"(whoami)"@"(hostname)":"(pwd)
    end

    if not functions -q __tj_fish_prompt_original
        functions --copy fish_prompt __tj_fish_prompt_original
    end
    function fish_prompt
        __tj_fish_prompt_original
        vterm_prompt_end
    end

    function clear --wraps clear
        vterm_printf "51;Evterm-clear-scrollback"
        command tput clear
    end
end

function fish_title
    if string match -q 'xterm*' -- "$TERM"
        echo (whoami)"@"(hostname)": "(prompt_pwd)
    else
        echo (prompt_pwd)
    end
end

if set -q EAT_SHELL_INTEGRATION_DIR
    if test -f "$EAT_SHELL_INTEGRATION_DIR/fish"
        source "$EAT_SHELL_INTEGRATION_DIR/fish"
    else if test -f "$EAT_SHELL_INTEGRATION_DIR/fish.fish"
        source "$EAT_SHELL_INTEGRATION_DIR/fish.fish"
    end
end

# goenv
if type -q goenv
    goenv init - | source

    set -q GOROOT; and __tj_prepend_path "$GOROOT/bin"
    set -q GOPATH; and __tj_append_path "$GOPATH/bin"
end

# pyenv
if type -q pyenv
    pyenv init - --no-rehash | source
    pyenv virtualenv-init - | source
end

# Homebrew (Linux)
if test -d /home/linuxbrew/.linuxbrew
    set -gx HOMEBREW_PREFIX /home/linuxbrew/.linuxbrew
    set -gx HOMEBREW_CELLAR /home/linuxbrew/.linuxbrew/Cellar
    set -gx HOMEBREW_REPOSITORY /home/linuxbrew/.linuxbrew/Homebrew

    __tj_prepend_path /home/linuxbrew/.linuxbrew/bin
    __tj_prepend_path /home/linuxbrew/.linuxbrew/sbin
    __tj_prepend_manpath /home/linuxbrew/.linuxbrew/share/man
    __tj_prepend_infopath /home/linuxbrew/.linuxbrew/share/info
end

function gam
    /home/tj/bin/gam/gam $argv
end

functions --erase __tj_prepend_path
functions --erase __tj_append_path
functions --erase __tj_prepend_manpath
functions --erase __tj_prepend_infopath
functions --erase __tj_load_dircolors

