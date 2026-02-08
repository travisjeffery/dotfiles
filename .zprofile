# Login-shell initialization.

# Homebrew (macOS)
if [[ -d "/opt/homebrew" ]]; then
  eval "$(/opt/homebrew/bin/brew shellenv)"
fi

# Linuxbrew
if [[ -f "/home/linuxbrew/.linuxbrew/bin/brew" ]]; then
  eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv zsh)"
fi

# Start ssh-agent once per login session (interactive only).
if [[ -o interactive ]]; then
  if [[ -z "$SSH_AUTH_SOCK" || ! -S "$SSH_AUTH_SOCK" ]]; then
    if [[ "$OSTYPE" == darwin* ]]; then
      export SSH_AUTH_SOCK="${TMPDIR%/}/ssh-agent.sock"
    else
      if [[ -n "$XDG_RUNTIME_DIR" && -d "$XDG_RUNTIME_DIR" ]]; then
        export SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/ssh-agent.sock"
      else
        export SSH_AUTH_SOCK="/tmp/ssh-agent-$USER.sock"
      fi
    fi
  fi

  if [[ ! -S "$SSH_AUTH_SOCK" ]]; then
    mkdir -p "${SSH_AUTH_SOCK:h}" 2>/dev/null
    chmod 700 "${SSH_AUTH_SOCK:h}" 2>/dev/null
    eval "$(ssh-agent -a "$SSH_AUTH_SOCK" -s)" >/dev/null
  fi
fi

