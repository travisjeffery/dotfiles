#!/usr/bin/env bash
# Pipes PreToolUse payloads through rm-guard.sh and checks each verdict.
# Usage: rm-guard-test.sh [path/to/rm-guard.sh]

set -uo pipefail

HOOK=${1:-$(dirname -- "$(realpath -- "$0")")/../rm-guard.sh}
work=$(mktemp -d -p "$HOME/.cache" rm-guard-test.XXXXXX)
trap 'rm -rf -- "$work"' EXIT
export RM_GUARD_LOG=$work/rm-guard.log
cwd=$work/project
export TMPDIR=$work/tmp
mkdir -p "$cwd" "$TMPDIR"; : > "$RM_GUARD_LOG"

pass=0 fail=0

# verdict COMMAND -> approve | allow | ask | block | error(RC)
# approve is an explicit allow decision; allow is no decision at all.
verdict() {
  local out rc
  out=$(jq -nc --arg c "$1" --arg d "$cwd" '{tool_name:"Bash",tool_input:{command:$c},cwd:$d}' |
    CLAUDE_PROJECT_DIR=$cwd TMPDIR=${T_TMPDIR:-$TMPDIR} bash "$HOOK" 2>/dev/null)
  rc=$?
  # The hook fails open, so a crash would otherwise read as allow.
  if (( rc == 2 )); then echo block
  elif (( rc != 0 )); then echo "error($rc)"
  elif [[ $out == *'"ask"'* ]]; then echo ask
  elif [[ $out == *'"allow"'* ]]; then echo approve
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

S=$TMPDIR/claude-$(id -u)/-home-user-project/abc/scratchpad

# Real commands that used to prompt.
expect allow "for-loop over repos in a scratchpad" \
  "S=$S; cd \$S/repos; for r in api frontend docs ml; do rm -rf \$r; gh repo clone example/\$r \$r -- --depth 1 2>&1 | tail -3; done" \
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

# Pattern removal on loop variables (a real stall).
expect allow "loop-derived scratch dir via \${rp%:*}" \
  "S=$S; for rp in frontend:10949 backend:16164; do r=\${rp%:*}; n=\${rp#*:}; d=\$S/rec-\$r; rm -rf \$d; mkdir -p \$d; gh pr diff \$n > \$d/diff; done" \
  "$S/rec-backend"
expect approve "derived values stay paired" \
  "for rp in a:1 b:2; do r=\${rp%:*}; n=\${rp#*:}; rm -rf $S/\$r-\$n; done" \
  "$S/b-2"
expect approve "%% and ## on a plain variable" \
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
expect approve "mktemp -p ~/.cache" \
  'D=$(mktemp -d -p ~/.cache); rm -rf "$D"' \
  "auto-approved"
expect approve "mktemp --tmpdir=\$HOME/.cache" \
  'D=$(mktemp -d --tmpdir=$HOME/.cache x.XXXX); rm -rf $D'
expect block "mktemp -dp /opt cluster" \
  'D=$(mktemp -dp /opt); rm -rf $D'
expect block "mktemp template relative to cwd outside roots" \
  'cd /opt && D=$(mktemp -d build.XXXX); rm -rf $D'
expect approve "plain mktemp" \
  'D=$(mktemp -d); rm -rf $D'
expect ask "mktemp -p with an unknown dir" \
  'D=$(mktemp -d -p $NOPE); rm -rf $D'

# Scratch deletes are approved outright (no permission prompt).
u=$(id -u)
mkdir -p "$S/clone/.git" "$TMPDIR/wt" "$TMPDIR/outer/a/wt" "$TMPDIR/real" /tmp/claude-rm-guard-test-$$ 2>/dev/null
echo 'gitdir: /elsewhere' > "$TMPDIR/wt/.git"
echo 'gitdir: /elsewhere' > "$TMPDIR/outer/a/wt/.git"
ln -s "$cwd" "$TMPDIR/to-project"
ln -s /opt "$TMPDIR/to-opt"
ln -s "$TMPDIR/real" "$TMPDIR/to-real"
trap 'rm -rf -- "$work" /tmp/claude-rm-guard-test-$$' EXIT

expect approve "quoted mktemp var" 'D=$(mktemp -d); rm -rf "$D"'
expect approve "mktemp, work, then cd and delete" \
  'D=$(mktemp -d) && mkdir -p "$D/x" && cd "$D" && rm -rf x; echo cleaned'
expect approve "mktemp file, rm -f" 'F=$(mktemp); rm -f -- "$F"'
expect approve "subpath of a mktemp dir" 'D=$(mktemp -d -t x.XXXX); rm -rf "$D/sub"'
expect approve "inside \$TMPDIR" 'rm -rf "$TMPDIR/build" $TMPDIR/cache'
expect approve "inside \${TMPDIR}" 'rm -rf ${TMPDIR}/build'
expect approve "inside a scratchpad" "rm -rf $S/out"
expect approve "inside ~/.cache/gotmp" 'rm -rf ~/.cache/gotmp/go-build123'
expect approve "inside /tmp/claude-*" "rm -rf /tmp/claude-rm-guard-test-$$/x"
expect approve "redirects to /dev/null and between fds" "rm -rf $S/out 2>/dev/null; rm -rf $S/b >/dev/null 2>&1 || true"
expect approve "rmdir in scratch" "rmdir $S/empty"
expect approve "glob in the last component" "rm -rf $S/*"
expect approve "contained .." "rm -rf $S/a/../b"
expect approve "symlink resolving inside scratch" 'rm -rf "$TMPDIR/to-real/x"'
expect approve "a clone (.git directory) in scratch" "rm -rf $S/clone"
expect approve "loop over scratch dirs" "for d in $S/a $S/b; do rm -rf \"\$d\"; done"
expect approve "grouped" "{ rm -rf $S/a; }"

# Not scratch: falls back to the normal decision.
expect allow "\$TMPDIR itself" 'rm -rf "$TMPDIR"'
expect allow "~/.cache/gotmp itself" 'rm -rf ~/.cache/gotmp'
expect allow "/tmp/claude-* itself" "rm -rf /tmp/claude-rm-guard-test-$$"
expect allow "Claude temp root" "rm -rf \$TMPDIR/claude-$u"
expect allow "a project's sessions" "rm -rf \$TMPDIR/claude-$u/-home-user-project"
expect allow "a whole session" "rm -rf \$TMPDIR/claude-$u/-home-user-project/abc"
expect allow ".. escaping the root" 'rm -rf $TMPDIR/a/../..'
expect allow ".. escaping to the root itself" 'rm -rf $TMPDIR/a/..'
expect allow "symlink out to the project" 'rm -rf $TMPDIR/to-project/x'
expect block "symlink out of the roots" 'rm -rf $TMPDIR/to-opt/x'
expect allow "glob before the last component" 'rm -rf $TMPDIR/*/x'
expect allow "brace expansion" "rm -rf $S/{a,..}"
expect allow "escaped space" "rm -rf $S/a\\ b"
expect block "split quoted word" "rm -rf \"$S/a ../../..\""
expect allow "value with whitespace" "X='$S/a $cwd'; rm -rf \$X"
expect allow "~user" "cd $S && rm -rf ~nobody/x"
expect allow "a git worktree" 'rm -rf $TMPDIR/wt'
expect allow "a dir holding a git worktree" 'rm -rf $TMPDIR/outer'
expect allow "mixing scratch and project" "rm -rf $S/a $cwd/x"
expect block "mixing scratch and outside" "rm -rf $S/a /opt/x"
expect allow "mixing scratch and outside, non-recursive" "rm -f $S/a /opt/x"
expect allow "other commands alongside" 'D=$(mktemp -d); curl -s example.com; rm -rf "$D"'
expect allow "piped" "rm -rf $S/a | cat"
expect allow "command substitution" "rm -rf $S/a; X=\$(id)"
expect allow "redirect to a file" "rm -rf $S/a > \$HOME/.profile"
expect allow "sudo" "sudo rm -rf $S/a"
expect allow "find -delete" "find $S -name '*.o' -delete"
expect allow "heredoc" $'rm -rf '"$S"$'/a; cat <<EOF\nx\nEOF'
expect allow "TMPDIR reassigned in the command" "TMPDIR=$cwd; rm -rf \$TMPDIR/x"
T_TMPDIR=$HOME expect allow "TMPDIR=\$HOME is not a scratch root" 'rm -rf ~/.cache/x'
expect allow "no delete at all" 'D=$(mktemp -d); echo "$D"'

# Unchanged behavior.
expect block "protected path" 'rm -rf ~/.ssh'
expect block "recursive outside roots" 'rm -r /opt/thing'
expect allow "inside cwd" 'rm -rf build'

printf '\n%d passed, %d failed\n' "$pass" "$fail"
(( fail == 0 ))
