#!/bin/sh
# Prepare a worker container's home on every start, then run the command
# (by default a headless Herdr server) as the worker user.
#
# Runtime inputs, all optional:
#   /run/qm-secrets/gh-token          GitHub token for gh and git over HTTPS
#   /run/qm-secrets/signing-key       this host's SSH commit-signing key (+ .pub)
#   /run/qm-secrets/beads-key         key for the forced-command Beads proxy
#   /run/qm-secrets/known_hosts       host keys the worker may SSH to
# Volumes under $HOME (src, worktrees, .cache, agent state) persist; config
# from /opt/dotfiles is re-applied here so the image, not the volume, owns it.
set -eu

user="${QM_USER:?}"
home="${HOME:?}"
secrets=/run/qm-secrets
private="$home/.config/qm-worker"

as_user() {
	if [ "$(id -u)" = 0 ]; then
		setpriv --reuid="$user" --regid="$user" --init-groups -- "$@"
	else
		"$@"
	fi
}

# Freshly created volumes are owned by root; give the mount points (not their
# contents) to the worker.
if [ "$(id -u)" = 0 ]; then
	for d in "$home" "$home/src" "$home/worktrees" "$home/.cache" "$home/.claude" \
		"$home/.codex" "$home/.config" "$home/.config/herdr" "$home/.config/muse"; do
		mkdir -p "$d"
		[ "$(stat -c %U "$d")" = "$user" ] || chown "$user:$user" "$d"
	done
fi

as_user sh -eu -s "$home" "$private" "$secrets" <<'SETUP'
home=$1 private=$2 secrets=$3
src=/opt/dotfiles

# Dotfiles: links, so an image update takes effect on restart.
for f in .gitconfig .claude/CLAUDE.md .claude/commands .claude/hooks \
	.codex/AGENTS.md .codex/herdr-agent-state.sh .codex/hooks .config/muse/plugins; do
	mkdir -p "$home/$(dirname "$f")"
	ln -sfn "$src/$f" "$home/$f"
done
# Agent settings are copied, not linked: the agents write to them.
cp "$src/worker/claude-settings.json" "$home/.claude/settings.json"
cp "$src/worker/codex-config.toml" "$home/.codex/config.toml"
cp "$src/worker/codex-hooks.json" "$home/.codex/hooks.json"
mkdir -p "$home/.config/herdr"
cp "$src/worker/herdr-config.toml" "$home/.config/herdr/config.toml"

# Credentials: copied out of the read-only mount into a private directory
# the worker owns. Nothing here is ever written back to the image.
umask 077
mkdir -p "$private"
for f in gh-token signing-key signing-key.pub beads-key known_hosts; do
	if [ -r "$secrets/$f" ]; then cp "$secrets/$f" "$private/$f"; else rm -f "$private/$f"; fi
done

{
	echo '[credential "https://github.com"]'
	echo '	helper ='
	echo '	helper = !gh auth git-credential'
	if [ -r "$private/signing-key.pub" ]; then
		echo '[gpg]'
		echo '	format = ssh'
		echo '[commit]'
		echo '	gpgsign = true'
		echo '[tag]'
		echo '	gpgsign = true'
	else
		echo '[commit]'
		echo '	gpgsign = false'
	fi
} >"$home/.gitconfig.local"

mkdir -p "$home/.ssh"
chmod 700 "$home/.ssh"
[ -r "$private/known_hosts" ] && cp "$private/known_hosts" "$home/.ssh/known_hosts"
:
SETUP

# The dotfiles' .gitconfig sets the desktop's signing key and signing after
# its include of .gitconfig.local, so this host's choice is set at a higher
# precedence: its own key, or no signing without one.
if [ -r "$private/signing-key.pub" ]; then
	export GIT_CONFIG_COUNT=1 GIT_CONFIG_KEY_0=user.signingKey GIT_CONFIG_VALUE_0="$private/signing-key.pub"
else
	export GIT_CONFIG_COUNT=2 GIT_CONFIG_KEY_0=commit.gpgsign GIT_CONFIG_VALUE_0=false \
		GIT_CONFIG_KEY_1=tag.gpgsign GIT_CONFIG_VALUE_1=false
fi
if [ -r "$private/gh-token" ]; then
	GH_TOKEN=$(cat "$private/gh-token")
	export GH_TOKEN
fi
cd "$home"
if [ "$(id -u)" = 0 ]; then
	exec setpriv --reuid="$user" --regid="$user" --init-groups -- "$@"
fi
exec "$@"
