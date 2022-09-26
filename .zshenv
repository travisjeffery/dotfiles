export PATH
export MANPATH
export INFOPATH

export GOENV_ROOT=$HOME/.goenv
export PATH="$GOENV_ROOT/bin:$PATH"

if type goenv &> /dev/null; then
  eval "$(goenv init -)"
fi

if [ -d "/usr/man" ]; then MANPATH="$MANPATH:/usr/man"; fi
if [ -d "/usr/share/man" ]; then MANPATH="$MANPATH:/usr/share/man"; fi
if [ -d "/usr/local/man" ]; then MANPATH="$MANPATH:/usr/local/man"; fi
if [ -d "/usr/local/share/man" ]; then MANPATH="/usr/local/share/man:$MANPATH"; fi
if [ -d "$HOME/man" ]; then MANPATH="$HOME/man:$MANPATH"; fi
if [ -d "$HOME/share/man" ]; then MANPATH="$HOME/share/man:$MANPATH"; fi

if [ -d "/usr/local/info" ]; then INFOPATH="/usr/local/info:$INFOPATH"; fi
if [ -d "/usr/info" ]; then INFOPATH="$INFOPATH:/usr/info"; fi
if [ -d "$HOME/info" ]; then INFOPATH="$HOME/info:$INFOPATH"; fi

if [ -d "/bin" ]; then PATH="$PATH":/bin; fi
if [ -d "/sbin" ]; then PATH="$PATH:/sbin"; fi
if [ -d "/usr/bin" ]; then PATH="$PATH:/usr/bin"; fi


if [ -d "/usr/local/bin" ]; then PATH="$PATH:/usr/local/bin"; fi
if [ -d "/usr/local/sbin" ]; then PATH="$PATH:/usr/local/sbin"; fi
if [ -d "$HOME/.cabal/bin" ]; then PATH="$HOME/.cabal/bin:$PATH"; fi
if [ -d "$HOME/.pulumi//bin" ]; then PATH="$HOME/.pulumi/bin:$PATH"; fi
if [ -d "$HOME/.lein/bin" ]; then PATH="$HOME/.lein/bin/:$PATH"; fi
if [ -d "$HOME/bin" ]; then PATH="$HOME/bin:$PATH"; fi


if [ -d "$HOME/.local/bin" ]; then PATH="$HOME/.local/bin:$PATH"; fi
if [ -d "$HOME/.local/share/JetBrains/Toolbox/scripts" ]; then PATH="$HOME/.local/share/JetBrains/Toolbox/scripts:$PATH"; fi
if [ -d "$HOME/.local/google-cloud-sdk/bin" ]; then PATH="$HOME/.local/google-cloud-sdk/bin:$PATH"; fi
if [ -d "$HOME/.cargo/bin" ]; then PATH="$HOME/.cargo/bin:$PATH"; fi
if [ -d "$HOME/.pulumi/bin" ]; then PATH="$HOME/.pulumi//bin:$PATH"; fi
if [ -d "/usr/local/protobuf/bin" ]; then PATH="/usr/local/protobuf/bin:$PATH"; fi
if [ -d "$HOME/.tfenv/bin" ]; then PATH="$HOME/.tfenv/bin:$PATH"; fi
if [ -d "$HOME/.local/istio/bin" ]; then PATH="$HOME/.local/istio/bin:$PATH"; fi
if [ -d "/opt/gradle/bin" ]; then PATH="/opt/gradle/bin:$PATH"; fi
if [ -d "/opt/chef/bin" ]; then PATH="/opt/chef/bin:$PATH"; fi
if [ -d "/opt/gradle" ]; then GRADLE_HOME="/opt/gradle"; fi
if [ -d "/opt/idea/bin" ]; then PATH="/opt/idea/bin:$PATH"; fi
if [ -d "/opt/tfenv/bin" ]; then PATH="/opt/tfenv/bin:$PATH"; fi
if [ -d "/opt/firefox" ]; then PATH="/opt/firefox:$PATH"; fi
if [ -d "/opt/goland" ]; then PATH="/opt/goland/bin:$PATH"; fi
if [ -d "/opt/visualvm" ]; then PATH="/opt/visualvm/bin:$PATH"; fi
if [ -d "$HOME/code/leiningen" ]; then PATH="$HOME/code/leiningen:$PATH"; fi
if [ -d "$HOME/dev/bin" ]; then PATH="$HOME/dev/bin:$PATH"; fi
if [ -d "/var/lib/snapd/snap/bin" ]; then PATH="/var/lib/snapd/snap/bin"; fi

command -v rustup >/dev/null && export RUST_SRC_PATH="$(rustc --print sysroot)/lib/rustlib/src/rust/library"

if [ -f /etc/os-release ]; then source /etc/os-release; fi

export EDITOR="emacs -q"
export PAGER=less
export SHELL=$(which zsh)
export TERM=xterm-256color
export GZIP='--best --name --verbose'
export GITHUB_USERNAME=travisjeffery
export JAVA_HOME=/usr
export JAVA_OPTS="-XX:+TieredCompilation -XX:TieredStopAtLevel=1 -Xverify:none"
export PATH=$PATH:$HOME/.gem/bin
export HELM_EXPERIMENTAL_OCI=1

alias pbcopy='xsel --clipboard --input'
alias pbpaste='xsel --clipboard --output'

export N_PREFIX="$HOME/n"; [[ :$PATH: == *":$N_PREFIX/bin:"* ]] || PATH+=":$N_PREFIX/bin"

source ~/work.sh

[ $TERM = "dumb" ] && unsetopt zle && PS1='$ '


