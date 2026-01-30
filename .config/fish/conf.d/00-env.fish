# Fish equivalents for settings in `~/.zshenv`.

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

function __tj_append_manpath --argument-names dir
    if test -n "$dir"; and test -d "$dir"; and not contains -- "$dir" $MANPATH
        set -gx MANPATH $MANPATH "$dir"
    end
end

function __tj_prepend_infopath --argument-names dir
    if test -n "$dir"; and test -d "$dir"; and not contains -- "$dir" $INFOPATH
        set -gx INFOPATH "$dir" $INFOPATH
    end
end

function __tj_append_infopath --argument-names dir
    if test -n "$dir"; and test -d "$dir"; and not contains -- "$dir" $INFOPATH
        set -gx INFOPATH $INFOPATH "$dir"
    end
end

# man/info paths
__tj_append_manpath /usr/man
__tj_append_manpath /usr/share/man
__tj_append_manpath /usr/local/man
__tj_prepend_manpath /usr/local/share/man
__tj_prepend_manpath $HOME/man
__tj_prepend_manpath $HOME/share/man

__tj_prepend_infopath /usr/local/info
__tj_append_infopath /usr/info
__tj_prepend_infopath $HOME/info

# PATH: append system dirs
__tj_append_path /bin
__tj_append_path /sbin
__tj_append_path /usr/bin
__tj_append_path /usr/local/bin
__tj_append_path /usr/local/sbin

# PATH: prepend user/tool dirs (keep ordering from `.zshenv`)
__tj_prepend_path $HOME/.cabal/bin
__tj_prepend_path $HOME/.pulumi/bin
__tj_prepend_path $HOME/.lein/bin
__tj_prepend_path $HOME/.npm/bin
__tj_prepend_path $HOME/bin

__tj_prepend_path $HOME/.local/bin
__tj_prepend_path $HOME/.local/share/JetBrains/Toolbox/scripts
__tj_prepend_path $HOME/.local/google-cloud-sdk/bin
__tj_prepend_path $HOME/.cargo/bin
__tj_prepend_path $HOME/.pulumi/bin
__tj_prepend_path /usr/local/protobuf/bin
__tj_prepend_path $HOME/.tfenv/bin
__tj_prepend_path $HOME/.linkerd2/bin
__tj_prepend_path $HOME/.local/istio/bin
__tj_prepend_path /opt/gradle/bin
__tj_prepend_path /opt/chef/bin
__tj_prepend_path /opt/idea/bin
__tj_prepend_path /opt/tfenv/bin
__tj_prepend_path /opt/firefox
__tj_prepend_path /opt/goland/bin
__tj_prepend_path /opt/visualvm/bin
__tj_prepend_path $HOME/code/leiningen
__tj_prepend_path $HOME/dev/bin
__tj_prepend_path /var/lib/snapd/snap/bin

if test -d /opt/gradle
    set -gx GRADLE_HOME /opt/gradle
end

__tj_append_path $HOME/.krew/bin

set -gx EDITOR emacsclient
set -gx PAGER less
set -gx SHELL (command -v fish)
set -gx GITHUB_USERNAME travisjeffery

set -gx GEM_PATH $HOME/.gem
set -gx GEM_HOME $HOME/.gem
__tj_append_path $HOME/.gem/bin

set -gx JAVA_HOME /usr
set -gx JAVA_OPTS "-XX:+TieredCompilation -XX:TieredStopAtLevel=1 -Xverify:none"
set -gx HELM_EXPERIMENTAL_OCI 1
set -gx KALEIDOSCOPE_DIR "$HOME/code/Kaleidoscope"
set -gx PULUMI_EXPERIMENTAL true
set -gx PULUMI_SKIP_CHECKPOINTS true

# node `n` prefix
set -gx N_PREFIX "$HOME/n"
__tj_append_path "$N_PREFIX/bin"

# Homebrew (macOS)
if test -d /opt/homebrew
    set -gx HOMEBREW_PREFIX /opt/homebrew
    set -gx HOMEBREW_CELLAR /opt/homebrew/Cellar
    set -gx HOMEBREW_REPOSITORY /opt/homebrew

    __tj_prepend_path /opt/homebrew/bin
    __tj_prepend_path /opt/homebrew/sbin
    __tj_prepend_manpath /opt/homebrew/share/man
    __tj_prepend_infopath /opt/homebrew/share/info
end

# Work-specific environment
# - fish can't `source` POSIX sh directly; prefer a fish script, or use `bass` if installed.
if test -f $HOME/work.fish
    source $HOME/work.fish
else if test -f $HOME/work.sh
    if functions -q bass
        bass source $HOME/work.sh
    end
end

# goenv/pyenv roots (init happens in `config.fish` for interactive shells)
set -gx GOENV_ROOT $HOME/.goenv
__tj_prepend_path "$GOENV_ROOT/bin"

set -gx PYENV_ROOT "$HOME/.pyenv"
__tj_prepend_path "$PYENV_ROOT/bin"

function __tj_ssh_agent --description 'Start ssh-agent on a stable per-user socket'
    if test (uname) = Darwin
        set -gx SSH_AUTH_SOCK "$TMPDIR/ssh-agent.sock"
    else if set -q XDG_RUNTIME_DIR; and test -d "$XDG_RUNTIME_DIR"
        set -gx SSH_AUTH_SOCK "$XDG_RUNTIME_DIR/ssh-agent.sock"
    else
        set -gx SSH_AUTH_SOCK "/tmp/ssh-agent-$USER.sock"
    end

    if not status is-interactive
        return 0
    end

    if test -S "$SSH_AUTH_SOCK"
        return 0
    end

    set -l sock_dir (dirname -- "$SSH_AUTH_SOCK")
    command mkdir -p "$sock_dir" ^/dev/null
    command chmod 700 "$sock_dir" ^/dev/null

    set -l agent_out (command ssh-agent -a "$SSH_AUTH_SOCK" -s ^/dev/null)
    for line in $agent_out
        if string match -q -r '^SSH_AUTH_SOCK=' -- $line
            set -gx SSH_AUTH_SOCK (string replace -r '^SSH_AUTH_SOCK=([^;]+);.*$' '$1' -- $line)
        else if string match -q -r '^SSH_AGENT_PID=' -- $line
            set -gx SSH_AGENT_PID (string replace -r '^SSH_AGENT_PID=([^;]+);.*$' '$1' -- $line)
        end
    end
end

__tj_ssh_agent

functions --erase __tj_prepend_path
functions --erase __tj_append_path
functions --erase __tj_prepend_manpath
functions --erase __tj_append_manpath
functions --erase __tj_prepend_infopath
functions --erase __tj_append_infopath
functions --erase __tj_ssh_agent
