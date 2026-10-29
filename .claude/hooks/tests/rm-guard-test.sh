#!/usr/bin/env bash
# Pipes PreToolUse payloads through rm-guard.sh and checks each verdict.
# Usage: rm-guard-test.sh [path/to/rm-guard.sh]

set -uo pipefail

HOOK=${1:-$(dirname -- "$(realpath -- "$0")")/../rm-guard.sh}
work=$(mktemp -d -p "$HOME/.cache" rm-guard-test.XXXXXX)
trap 'rm -rf -- "$work"' EXIT
export RM_GUARD_LOG=$work/rm-guard.log
cwd=$work/project
mkdir -p "$cwd"; : > "$RM_GUARD_LOG"

pass=0 fail=0

# verdict COMMAND -> allow | ask | block | error(RC)
verdict() {
  local out rc
  out=$(jq -nc --arg c "$1" --arg d "$cwd" '{tool_name:"Bash",tool_input:{command:$c},cwd:$d}' |
    CLAUDE_PROJECT_DIR=$cwd bash "$HOOK" 2>/dev/null)
  rc=$?
  # The hook fails open, so a crash would otherwise read as allow.
  if (( rc == 2 )); then echo block
  elif (( rc != 0 )); then echo "error($rc)"
  elif [[ $out == *'"ask"'* ]]; then echo ask
  else echo allow
  fi
}

# expect WANT NAME COMMAND [LOG_PATTERN]: LOG_PATTERN must appear in a new log line.
expect() {
  local want=$1 name=$2 cmd=$3 pat=${4-} got before
  before=$(wc -l < "$RM_GUARD_LOG" 2>/dev/null || echo 0)
  got=$(verdict "$cmd")
  if [[ $got != "$want" ]]; then
    printf 'FAIL %s: want %s, got %s\n' "$name" "$want" "$got"; (( fail++ )); return
  fi
  if [[ -n $pat ]] && ! tail -n +"$((before + 1))" "$RM_GUARD_LOG" 2>/dev/null | grep -qF -- "$pat"; then
    printf 'FAIL %s: %s, but no log line matching %q\n' "$name" "$got" "$pat"; (( fail++ )); return
  fi
  printf 'ok   %s: %s\n' "$name" "$got"; (( pass++ ))
}

S=$HOME/.cache/tmp/claude-1000/-home-tj-worktrees-backend-x/abc/scratchpad

# Today's real commands, which used to prompt.
expect allow "for-loop over repos in a scratchpad" \
  "S=$S; cd \$S/repos; for r in geo-network-service frontend oscilar-promo-video ml; do rm -rf \$r; gh repo clone Oscilar/\$r \$r -- --depth 1 2>&1 | tail -3; done" \
  "$S/repos/frontend"
expect allow "mktemp -p ~/.cache DOCKER_CONFIG" \
  'cd ~/.cache && export DOCKER_CONFIG=$(mktemp -d -p ~/.cache); crane manifest x; rm -rf $DOCKER_CONFIG' \
  "$HOME/.cache/rm-guard-mktemp"

expect allow "for-loop with quoted words" \
  "cd $S; for r in 'a b' \"c\"; do rm -rf \"\$r\"; done"
expect allow "for-loop words with variables" \
  "S=$S; for r in \$S/a \${S}/b ~/.cache/c; do rm -r \$r; done"
expect block "for-loop reaching outside roots" \
  "cd $S; for r in a /etc/nginx; do rm -rf \$r; done"
expect block "for-loop with a relative escape" \
  "cd $cwd; for r in ok ../../../../../../opt; do rm -rf \$r; done"
expect ask "for-loop with a glob" \
  "cd $S; for r in *; do rm -rf \$r; done"
expect ask "for-loop with command substitution" \
  "cd $S; for r in \$(ls); do rm -rf \$r; done"
expect ask "for-loop with an unknown variable" \
  "for r in \$NOPE/x; do rm -rf \$r; done"
expect block "for-loop hitting a protected path" \
  "for r in $S/x ~/.ssh; do rm -rf \$r; done"
expect ask "loop variable reassigned to unknown" \
  "for r in a; do r=\$(pwd); rm -rf \$r; done"
expect ask "stale value doesn't survive a rebind" \
  "r=$S/x; for r in \$(ls); do rm -rf \$r; done"

# Pattern removal on loop variables (backend-jet1 stuck on this shape).
expect allow "loop-derived scratch dir via \${rp%:*}" \
  "S=$S; for rp in frontend:10949 backend:16164; do r=\${rp%:*}; n=\${rp#*:}; d=\$S/rec-\$r; rm -rf \$d; mkdir -p \$d; gh pr diff \$n > \$d/diff; done" \
  "$S/rec-backend"
expect allow "derived values stay paired" \
  "for rp in a:1 b:2; do r=\${rp%:*}; n=\${rp#*:}; rm -rf $S/\$r-\$n; done" \
  "$S/b-2"
expect allow "%% and ## on a plain variable" \
  "S=$S/x/y; rm -rf \${S%%/x*}/z \$S/\${S##*/}"
expect block "derived value outside roots" \
  "for rp in /opt/a:1 $S/b:2; do r=\${rp%:*}; rm -rf \$r; done"
expect block "pattern removal on HOME leaves the roots" \
  'rm -rf ${HOME%/*}'
expect ask "derived value with .." \
  "for rp in ..:1; do r=\${rp%:*}; rm -rf $S/\$r; done"
expect ask "pattern with a variable" \
  "for rp in a:1; do r=\${rp%\$X}; rm -rf $S/\$r; done"
expect ask "pattern on an unknown variable" \
  "rm -rf $S/\${NOPE%:*}"
expect ask "derived through command substitution" \
  "for rp in a:1; do r=\$(basename \$rp); rm -rf $S/\$r; done"
expect ask "dependency redefined after the template" \
  "for rp in a:1; do x=/opt/\${rp%:*}; d=\$x; x=$S/safe; rm -rf \$d; done"
expect ask "self-referencing loop variable" \
  "for r in $S/a/; do r=\${r%/}; rm -rf \$r; done"

expect block "mktemp -p /etc, recursive" \
  'D=$(mktemp -d -p /etc); rm -rf $D'
expect allow "mktemp -p ~/.cache" \
  'D=$(mktemp -d -p ~/.cache); rm -rf "$D"' \
  "$HOME/.cache/rm-guard-mktemp"
expect allow "mktemp --tmpdir=\$HOME/.cache" \
  'D=$(mktemp -d --tmpdir=$HOME/.cache x.XXXX); rm -rf $D'
expect block "mktemp -dp /opt cluster" \
  'D=$(mktemp -dp /opt); rm -rf $D'
expect block "mktemp template relative to cwd outside roots" \
  'cd /opt && D=$(mktemp -d build.XXXX); rm -rf $D'
expect allow "plain mktemp still /tmp" \
  'D=$(mktemp -d); rm -rf $D'
expect ask "mktemp -p with an unknown dir" \
  'D=$(mktemp -d -p $NOPE); rm -rf $D'

# Unchanged behavior.
expect block "protected path" 'rm -rf ~/.ssh'
expect block "recursive outside roots" 'rm -r /opt/thing'
expect allow "inside cwd" 'rm -rf build'

printf '\n%d passed, %d failed\n' "$pass" "$fail"
(( fail == 0 ))
