#!/usr/bin/env bash
# rm-guard.sh — PreToolUse guard for Bash, scoped to file deletion.
#
# Runs alongside block-dangerous-bash.py, which vetoes `rm -rf` only when BOTH
# flags are present. That leaves `rm -r` (no -f), `rm -f` (no -r), and
# `find -delete` unguarded; this closes those and adds paths that must never be
# deleted regardless of flags or cwd.
#
# Graded response: exit 2 hard-blocks the irreversible cases (protected paths,
# recursive deletes outside the allowed roots). Everything else is allowed,
# except a recursive delete of a path that can't be resolved even after
# expanding the command's own variables, which stays a prompt. Allowed cases
# that used to prompt are appended to $RM_GUARD_LOG for auditing.
#
# Variables are resolved from assignments earlier in the same command
# (`W=$(mktemp -d)`, `export GOTMPDIR=$HOME/.cache/gotmp`) and from a short
# list of environment variables, so a delete of `$W/x` is judged by where it
# really points. `cd DIR` segments move the base for relative paths.
# `for NAME in WORDS` binds NAME to every word, and a delete through it must be
# safe for each one. `mktemp` values point into the directory mktemp would use.
#
# A command made only of deletes inside scratch space (see SCRATCH_ROOTS) plus
# plumbing such as assignments, cd and mkdir is approved outright, so an
# unattended session doesn't stall on a prompt for cleaning up its own temp
# dirs. The approval covers the whole command, which is why anything else in it
# (another program, a redirect, a command substitution other than mktemp)
# leaves the decision to the normal permission flow.
#
# Internal errors fail OPEN (exit 0), matching block-dangerous-bash.py: this
# guards against accidents, not an adversary, and a crashing hook that blocked
# every command would wedge the session.

set -uo pipefail

# Deletions may only reach inside these. $CWD/$PROJECT resolved from the payload.
ALLOWED_ROOTS=('$CWD' '$PROJECT' /tmp /var/tmp "$HOME/.cache")

# Never deletable, even inside an allowed root.
PROTECTED=("$HOME/.ssh" "$HOME/.aws" "$HOME/.gnupg" "$HOME/.config" "$HOME/.claude")

# Deletes strictly inside these are approved without a prompt; the roots
# themselves are not. A '*' matches one path component. Directories mktemp
# creates in the same command count too. Taken from the hook's environment, so
# a command can't widen them by assigning TMPDIR.
SCRATCH_ROOTS=("${TMPDIR:-}" '/tmp/claude-*' "${TMPDIR:-/tmp}/claude-$(id -u)" "$HOME/.cache/gotmp")

# Claude Code keeps each session's files at CLAUDE_TMP/PROJECT/SESSION; those
# two levels are shared with other sessions, so only what's below them is scratch.
CLAUDE_TMP=${TMPDIR:-/tmp}/claude-$(id -u)

# Never approved as scratch, nor is anything that contains them.
KEEP=("$HOME" /data/worktrees /data/gyb)

# Environment variables trusted to expand when the command doesn't set them.
ENV_VARS=(HOME TMPDIR USER XDG_RUNTIME_DIR XDG_CACHE_HOME XDG_STATE_HOME XDG_CONFIG_HOME XDG_DATA_HOME CLAUDE_PROJECT_DIR)

RM_GUARD_LOG=${RM_GUARD_LOG:-$HOME/.cache/rm-guard.log}

die_open() { exit 0; }

block() {
  printf 'Blocked by rm-guard: %s\n' "$1" >&2
  exit 2
}

allow() {
  jq -nc --arg r "$1" '{hookSpecificOutput:{
    hookEventName:"PreToolUse",
    permissionDecision:"allow",
    permissionDecisionReason:$r}}'
  exit 0
}

ask() {
  jq -nc --arg r "$1" '{hookSpecificOutput:{
    hookEventName:"PreToolUse",
    permissionDecision:"ask",
    permissionDecisionReason:$r}}'
  exit 0
}

# A case that used to prompt and is now allowed: record it, don't interrupt.
note_allowed() {
  { mkdir -p -- "$(dirname -- "$RM_GUARD_LOG")" &&
    printf '%s\tallowed\t%s\t%s\n' "$(date -Is)" "$1" "${cmd//$'\n'/ }" >>"$RM_GUARD_LOG"; } 2>/dev/null
  return 0
}

payload=$(cat) || die_open
command -v jq >/dev/null 2>&1 || die_open

tool=$(printf '%s' "$payload" | jq -r '.tool_name // empty' 2>/dev/null) || die_open
[[ $tool == Bash ]] || die_open

cmd=$(printf '%s' "$payload" | jq -r '.tool_input.command // empty' 2>/dev/null) || die_open
[[ -n $cmd ]] || die_open

cwd=$(printf '%s' "$payload" | jq -r '.cwd // empty' 2>/dev/null)
[[ -n $cwd ]] || cwd=$PWD
project=${CLAUDE_PROJECT_DIR:-$cwd}

roots=()
for r in "${ALLOWED_ROOTS[@]}"; do
  case $r in
    '$CWD') r=$cwd ;;
    '$PROJECT') r=$project ;;
  esac
  [[ -n $r ]] && roots+=("$(realpath -m -- "$r" 2>/dev/null || printf '%s' "$r")")
done

# A root at or above $HOME (TMPDIR=$HOME, TMPDIR=/) would make everything scratch.
scratch_roots=()
for r in "${SCRATCH_ROOTS[@]}"; do
  [[ -n $r && $r == /* ]] || continue
  [[ $r == *'*'* ]] || r=$(realpath -m -- "$r" 2>/dev/null) || continue
  [[ $HOME == "$r" || $HOME == "${r%/}"/* ]] && continue
  scratch_roots+=("$r")
done

# Marks paths mktemp creates in this command; see mktemp_path.
mktemp_mark=rm-guard-mktemp.$RANDOM$RANDOM

# Variables assigned earlier in this command, name -> value.
declare -A vars=()
# Loop variables bound by `for NAME in WORDS`, name -> newline-separated words.
declare -A loopvals=()
declare -A newly=()
# Assignments built from a loop variable, name -> raw value, evaluated per binding.
declare -A derived=()
derived_order=()
# Base directory for relative paths; follows `cd` segments.
base=$cwd

# Heredoc bodies are data. Without this, a commit message that merely mentions
# `rm -rf` trips the guard.
strip_heredocs() {
  local line delim="" trimmed
  while IFS= read -r line || [[ -n $line ]]; do
    if [[ -n $delim ]]; then
      trimmed=${line#"${line%%[![:space:]]*}"}
      [[ $trimmed == "$delim" ]] && delim=""
      continue
    fi
    printf '%s\n' "$line"
    if [[ $line =~ \<\<-?[[:space:]]*[\'\"]?([A-Za-z_][A-Za-z0-9_]*) ]]; then
      delim=${BASH_REMATCH[1]}
    fi
  done
}

# `cd /tmp && rm -r x` must be checked piecewise, but the && inside
# `echo "a && b"` is data and must not split, and neither is a separator
# inside `$(...)`.
split_segments() {
  local s=$1 buf="" quote="" esc=0 depth=0 i c two
  for (( i=0; i<${#s}; i++ )); do
    c=${s:i:1}
    if (( esc )); then buf+=$c; esc=0; continue; fi
    if [[ $c == '\' ]]; then buf+=$c; esc=1; continue; fi
    if [[ $quote == "'" ]]; then
      buf+=$c
      [[ $c == "'" ]] && quote=""
      continue
    fi
    two=${s:i:2}
    if [[ $two == '$(' ]]; then buf+=$two; (( depth++, i++ )); continue; fi
    if (( depth )); then
      buf+=$c
      [[ $c == '(' ]] && (( depth++ ))
      [[ $c == ')' ]] && (( depth-- ))
      continue
    fi
    if [[ $quote == '"' ]]; then
      buf+=$c
      [[ $c == '"' ]] && quote=""
      continue
    fi
    if [[ $c == '"' || $c == "'" ]]; then quote=$c; buf+=$c; continue; fi
    if [[ $two == "&&" || $two == "||" ]]; then
      printf '%s\n' "$buf"; buf=""; (( i++ )); continue
    fi
    # 2>&1, >&2 and &>file are redirections, not background jobs.
    if [[ $c == '&' && ( ${s:i-1:1} == [\<\>] || ${s:i+1:1} == '>' ) ]] && (( i )); then
      buf+=$c; continue
    fi
    if [[ $c == ';' || $c == '|' || $c == '&' ]]; then
      printf '%s\n' "$buf"; buf=""; continue
    fi
    buf+=$c
  done
  printf '%s\n' "$buf"
}

unquote() {
  local t=$1
  [[ ${#t} -ge 2 && ${t:0:1} == '"' && ${t: -1} == '"' ]] && t=${t:1:${#t}-2}
  [[ ${#t} -ge 2 && ${t:0:1} == "'" && ${t: -1} == "'" ]] && t=${t:1:${#t}-2}
  # Stray quotes left by whitespace tokenizing ("$W"/x) carry no meaning here.
  t=${t//\"/}
  printf '%s' "$t"
}

env_value() {
  local n=$1 e
  for e in "${ENV_VARS[@]}"; do
    [[ $e == "$n" ]] && { printf '%s' "${!n-}"; return 0; }
  done
  return 1
}

# ${NAME%pat} and friends for a NAME with a known value. Patterns are limited
# to literal path characters plus * and ?, which mean the same in bash and zsh;
# anything else, and any '..', stays unresolved.
strip_pattern() {
  local name=$1 op=$2 pat=$3 val
  if [[ -n ${vars[$name]+x} ]]; then val=${vars[$name]}
  else val=$(env_value "$name") && [[ -n $val ]] || return 1
  fi
  [[ $pat =~ ^[A-Za-z0-9_.:/@+=,*?-]*$ ]] || return 1
  [[ $val == *..* || $pat == *..* ]] && return 1
  case $op in
    %)  val=${val%$pat} ;;
    %%) val=${val%%$pat} ;;
    '#')  val=${val#$pat} ;;
    '##') val=${val##$pat} ;;
  esac
  printf '%s' "$val"
}

# Expand $NAME, ${NAME}, ${NAME:-default} and ${NAME%pat} (%, %%, #, ##) from
# this command's assignments, then trusted env. Anything left unexpanded stays
# as-is, so the caller still sees a '$'.
re_param='^([^$]*)\$(\{([A-Za-z_][A-Za-z0-9_]*)((:?-)([^}]*)|(%%|%|##|#)([^}]*))?\}|([A-Za-z_][A-Za-z0-9_]*))(.*)$'
expand() {
  local s=$1 out="" name val def hasdef op pat
  while [[ $s =~ $re_param ]]; do
    out+=${BASH_REMATCH[1]}
    name=${BASH_REMATCH[3]:-${BASH_REMATCH[9]}}
    hasdef=${BASH_REMATCH[5]:+1}
    def=${BASH_REMATCH[6]}
    op=${BASH_REMATCH[7]} pat=${BASH_REMATCH[8]}
    s=${BASH_REMATCH[10]}
    if [[ -n $op ]]; then
      # The regex may split %% as % plus a pattern starting with %.
      [[ $op == % && $pat == %* ]] && { op=%%; pat=${pat#%}; }
      [[ $op == '#' && $pat == '#'* ]] && { op='##'; pat=${pat#\#}; }
      val=$(strip_pattern "$name" "$op" "$pat") || { out+="\$${BASH_REMATCH[2]}"; continue; }
    elif [[ -n ${vars[$name]+x} ]]; then
      val=${vars[$name]}
    elif val=$(env_value "$name") && [[ -n $val ]]; then
      :
    elif [[ -n $hasdef && $def != *'$'* ]]; then
      # ${NAME:-default} with NAME unknown: judge by the default only when the
      # variable is one we'd otherwise trust (e.g. ${TMPDIR:-/tmp}).
      env_value "$name" >/dev/null || { out+="\$${BASH_REMATCH[2]}"; continue; }
      val=$def
    else
      out+="\$${BASH_REMATCH[2]}"
      continue
    fi
    out+=$val
  done
  printf '%s' "$out$s"
}

# Expand ~ and variables in a path word; fail if anything stays unknown.
expand_path() {
  local v=$1
  [[ $v == '~' ]] && v=$HOME
  [[ $v == '~/'* ]] && v=$HOME/${v#\~/}
  [[ $v == *'$('* || $v == *'`'* ]] && return 1
  v=$(expand "$v")
  [[ $v == *'$'* ]] && return 1
  printf '%s' "$v"
}

# Where `mktemp ARGS` creates its file, following GNU mktemp: -t prefers
# $TMPDIR over -p DIR; -p DIR/--tmpdir=DIR otherwise wins, then $TMPDIR, then
# /tmp. Without -p/-t/--tmpdir, a template is relative to the current directory.
mktemp_path() {
  local -a args; read -ra args <<< "$1"
  local a i j dir="" usedir=0 t_opt=0 template="" envtmp
  for (( i=0; i<${#args[@]}; i++ )); do
    a=$(unquote "${args[i]}")
    case $a in
      --) (( i+1 < ${#args[@]} )) && template=$(unquote "${args[i+1]}"); break ;;
      --tmpdir) usedir=1 ;;
      --tmpdir=*) usedir=1; dir=${a#--tmpdir=} ;;
      --suffix) (( i++ )) ;;
      --*) ;;
      -?*)
        for (( j=1; j<${#a}; j++ )); do
          case ${a:j:1} in
            t) t_opt=1 ;;
            p) usedir=1
               if (( j+1 < ${#a} )); then dir=${a:j+1}; else (( i++ )); dir=$(unquote "${args[i]-}"); fi
               break ;;
          esac
        done
        ;;
      *) template=$a ;;
    esac
  done
  if (( usedir || t_opt )) || [[ -z $template ]]; then
    envtmp=$(expand '$TMPDIR'); [[ $envtmp == *'$'* ]] && envtmp=""
    if (( t_opt )) && [[ -n $envtmp ]]; then dir=$envtmp; fi
    [[ -n $dir ]] || dir=${envtmp:-/tmp}
    dir=$(expand_path "$dir") || return 1
    [[ $template == */* ]] && dir=$dir/${template%/*}
  elif [[ $template == */* ]]; then
    dir=$(expand_path "${template%/*}") || return 1
  else
    dir=.
  fi
  [[ $dir == /* ]] || dir=$base/$dir
  printf '%s/%s' "$dir" "$mktemp_mark"
}

# The value an assignment gives its variable, or empty when it can't be known.
assigned_value() {
  local v
  v=$(unquote "$1")
  if [[ $v =~ ^(\$\(|\`)mktemp([[:space:]].*)?(\)|\`)$ ]]; then
    mktemp_path "${BASH_REMATCH[2]}"
    return
  fi
  expand_path "$v"
}

# Record NAME=value tokens from a segment's leading assignments or an
# export/local/declare/readonly builtin.
record_assignments() {
  local t name
  for t in "$@"; do
    [[ $t =~ ^([A-Za-z_][A-Za-z0-9_]*)=(.*)$ ]] || continue
    set_var "${BASH_REMATCH[1]}" "${BASH_REMATCH[2]}"
  done
}

set_var() {
  local name=$1 deps
  deps=$'\n'$(depends_on "$2")$'\n'
  forget "$name"
  # A value built from a loop variable (`r=${rp%:*}`) is kept as a template and
  # evaluated per binding by loop_expand. One that refers to itself stays unknown.
  if [[ $deps == *$'\n'[LD]' '* ]]; then
    [[ $deps == *$'\n'X$'\n'* || $deps == *$'\n'[LD]" $name"$'\n'* ]] && return
    derived[$name]=$2
    derived_order+=("$name")
    newly[$name]=1
    return
  fi
  vars[$name]=$(assigned_value "$2")
  [[ -n ${vars[$name]} ]] || { unset "vars[$name]"; return; }
  [[ $2 == *mktemp*' -p'* || $2 == *mktemp*--tmpdir* ]] && newly[$name]=1
}

# Drop every binding of NAME and every template built on it: those captured
# NAME's old value, which this guard no longer knows.
forget() {
  local name=$1 n
  unset "vars[$name]" "loopvals[$name]" "newly[$name]" "derived[$name]"
  for n in "${!derived[@]}"; do
    [[ -n ${derived[$n]+x} ]] && refs_var "${derived[$n]}" "$name" && forget "$n"
  done
  return 0
}

# What a raw word depends on, one per line: "L NAME" for a loop variable,
# "D NAME" for a template, following templates; "X" past a depth limit.
depends_on() {
  local s=$1 depth=${2:-0} n
  (( depth > 8 )) && { echo X; return; }
  for n in "${!loopvals[@]}"; do refs_var "$s" "$n" && echo "L $n"; done
  for n in "${!derived[@]}"; do
    refs_var "$s" "$n" || continue
    echo "D $n"
    depends_on "${derived[$n]}" $((depth + 1))
  done
  return 0
}

# `for NAME in WORDS`: bind NAME to each word, expanded now. A list with a
# glob, command substitution or unknown variable leaves NAME unresolvable.
word_re='^[[:space:]]*("[^"]*"|'"'"'[^'"'"']*'"'"'|[^[:space:]]+)(.*)$'
bind_for() {
  local name=$1 words=$2 w vals=""
  forget "$name"
  while [[ $words =~ $word_re ]]; do
    w=${BASH_REMATCH[1]}; words=${BASH_REMATCH[2]}
    [[ $w == *[*?[]* ]] && return
    w=$(expand_path "$(unquote "$w")") || return
    [[ -n $w ]] || return
    vals+=$w$'\n'
  done
  [[ -n $vals ]] || return
  loopvals[$name]=$vals
  newly[$name]=1
}

refs_var() {
  [[ $1 =~ \$(\{$2[}:%#]|$2([^A-Za-z0-9_]|$)) ]]
}

# Every concrete target a raw target stands for, one per line: for each
# combination of bindings of the loop variables it depends on, evaluate the
# templates in definition order, then expand the target.
loop_expand() {
  local raw=$1 deps
  local -a loops
  deps=$(depends_on "$raw")
  if [[ -z $deps ]]; then printf '%s\n' "$raw"; return; fi
  if [[ $'\n'$deps$'\n' == *$'\n'X$'\n'* ]]; then printf '%s\n' "$raw"; return; fi
  mapfile -t loops < <(sed -n 's/^L //p' <<< "$deps" | sort -u)
  bind_each 0
}

bind_each() {
  local i=$1 v n
  if (( i == ${#loops[@]} )); then
    (
      for n in "${derived_order[@]}"; do
        [[ -n ${derived[$n]+x} ]] || continue
        v=$(assigned_value "${derived[$n]}") && [[ -n $v ]] && vars[$n]=$v
      done
      expand "$raw"; echo
    )
    return
  fi
  while IFS= read -r v; do
    [[ -n $v ]] || continue
    (vars[${loops[i]}]=$v; bind_each $((i + 1)))
  done <<< "${loopvals[${loops[i]}]}"
}

# Names whose resolution is new to this guard: a delete they allow used to
# prompt, so it's logged.
newly_refs() {
  local n
  for n in "${!newly[@]}"; do refs_var "$1" "$n" && return 0; done
  return 1
}

# Consume NAME=value assignments from the front of a segment string, where a
# value may be $(...), `...` or quoted and contain spaces. Leaves the rest in
# $rest (not printed: a subshell would lose the variables it sets).
re_assign='^[[:space:]]*([A-Za-z_][A-Za-z0-9_]*)=(\$\([^)]*\)|`[^`]*`|"[^"]*"|'"'"'[^'"'"']*'"'"'|[^[:space:]]*)(.*)$'
rest=''
take_assignments() {
  rest=$1
  local name value
  while [[ $rest =~ $re_assign ]]; do
    name=${BASH_REMATCH[1]} value=${BASH_REMATCH[2]} rest=${BASH_REMATCH[3]}
    set_var "$name" "$value"
  done
}

inside() {
  local path=$1 root=$2
  [[ $path == "$root" || $path == "$root"/* ]]
}

# Resolve a raw target to an absolute path, or fail when it can't be known.
resolve() {
  local raw
  raw=$(unquote "$1")
  [[ $raw == '~' ]] && raw=$HOME
  [[ $raw == '~/'* ]] && raw=$HOME/${raw#\~/}
  raw=$(expand "$raw")
  [[ $raw == *'$'* || $raw == *'`'* ]] && return 1
  [[ $raw == /* ]] || raw=$base/$raw
  realpath -m -- "$raw" 2>/dev/null
}

classify_target() {
  local resolved
  resolved=$(resolve "$1") || { printf 'unknown'; return; }

  [[ $resolved == / ]] && { printf 'protected'; return; }

  local p
  for p in "${PROTECTED[@]}"; do
    inside "$resolved" "$(realpath -m -- "$p" 2>/dev/null || printf '%s' "$p")" \
      && { printf 'protected'; return; }
  done
  [[ $(basename -- "$resolved") == .git ]] && { printf 'protected'; return; }

  local r
  for r in "${roots[@]}"; do
    inside "$resolved" "$r" && { printf 'allowed'; return; }
  done
  printf 'outside'
}

# Whether bash will delete exactly the path rm-guard resolved, and that path
# is scratch. Quoting, escapes, brace expansion, word splitting and a glob
# before the last component can each make the real target differ.
is_scratch() {
  local raw=$1 t=$2 path r mark_re
  [[ $raw == *\\* ]] && return 1
  local dq=${raw//[^\"]/} sq=${raw//[^\']/}
  (( ${#dq} % 2 == 0 && ${#sq} % 2 == 0 )) || return 1
  t=$(unquote "$t")
  [[ $t == '~'[!/]* ]] && return 1
  t=$(expand "$t")
  [[ $t == *[[:space:]{}]* ]] && return 1
  path=$(resolve "$2") || return 1
  [[ ${path%/*} == *[*?[]* ]] && return 1

  for r in "${KEEP[@]}"; do
    r=$(realpath -m -- "$r" 2>/dev/null) || return 1
    inside "$r" "$path" && return 1
  done
  r=$(realpath -m -- "$CLAUDE_TMP" 2>/dev/null) || return 1
  if inside "$path" "$r"; then
    [[ ${path#"$r"} == /*/*/* ]] || return 1
  fi
  # A git worktree (its .git is a file) holds a branch registered elsewhere.
  if [[ -e $path ]]; then
    [[ -f $path/.git ]] && return 1
    [[ -d $path && -n $(find "$path" -maxdepth 4 -name .git -type f -print -quit 2>/dev/null) ]] && return 1
  fi

  mark_re="/$mktemp_mark(/|\$)"
  [[ $path =~ $mark_re ]] && return 0
  for r in "${scratch_roots[@]}"; do
    # Unquoted so a '*' in the root matches; the path is strictly below it.
    # shellcheck disable=SC2053
    [[ $path == $r/?* ]] && return 0
  done
  return 1
}

resolve_display() {
  resolve "$1" || unquote "$1"
}

# Whether the command so far is only scratch deletes and plumbing, and how
# many delete targets it has.
plain=1 deletes=0

note_scratch() {
  (( deletes++ ))
  [[ $1 == allowed ]] && is_scratch "$2" "$3" || plain=0
}

check_targets() {
  local recursive=$1 label=$2; shift 2
  local raw t verdict
  for raw in "$@"; do
   while IFS= read -r t; do
    verdict=$(classify_target "$t")
    note_scratch "$verdict" "$raw" "$t"
    case $verdict in
      protected)
        block "$label targets $(resolve_display "$t"), which is a protected path. Delete it yourself if you truly mean to."
        ;;
      outside)
        if (( recursive )); then
          block "$label recursively targets $(resolve_display "$t"), outside the allowed roots (${roots[*]}). Delete it yourself if intended."
        fi
        note_allowed "$label non-recursive outside roots: $(resolve_display "$t")"
        ;;
      unknown)
        if (( recursive )); then
          ask "$label recursively targets '$t', which contains a variable rm-guard cannot resolve from this command."
        fi
        note_allowed "$label non-recursive unresolved: $t"
        ;;
      allowed)
        (( recursive )) && newly_refs "$raw" && note_allowed "$label resolved $raw: $(resolve_display "$t")"
        ;;
    esac
   done < <(loop_expand "$raw")
  done
}

# Whether a segment can only do what its words say: no command substitution
# other than a plain $(mktemp ...), no process substitution, and no redirect
# except to /dev/null or between fds.
plain_segment() {
  local s=$1 re='\$\(mktemp([[:space:]][-[:alnum:]_./~=:${}[:space:]]*)?\)'
  while [[ $s =~ $re ]]; do s=${s/"${BASH_REMATCH[0]}"/}; done
  s=$(sed -E 's#(^|[[:space:]])([0-9]*|&)>>?[[:space:]]*/dev/null([[:space:]]|$)#\1\3#g; s#[0-9]*>&[0-9-]##g' <<< "$s")
  [[ $s != *'$('* && $s != *'`'* && $s != *'<'* && $s != *'>'* ]]
}

while IFS= read -r seg; do
  [[ -n ${seg//[[:space:]]/} ]] || continue
  plain_segment "$seg" || plain=0

  # Shell keywords that can start a segment (`for d in *; do rm -r "$d"`).
  while [[ $seg =~ ^[[:space:]]*(do|then|else|elif|if|while|until|!)[[:space:]]+(.*)$ ]]; do
    seg=${BASH_REMATCH[2]}
  done

  # Leading VAR=value assignments set variables for later segments; a bare
  # assignment segment (`W=$(mktemp -d)`) is nothing else. export/local/...
  # assignments are parsed the same way.
  if [[ $seg =~ ^[[:space:]]*for[[:space:]]+([A-Za-z_][A-Za-z0-9_]*)([[:space:]]+in([[:space:]]+(.*))?)?[[:space:]]*$ ]]; then
    bind_for "${BASH_REMATCH[1]}" "${BASH_REMATCH[4]}"
    continue
  fi

  take_assignments "$seg"; seg=$rest
  if [[ $seg =~ ^[[:space:]]*(export|local|declare|readonly|typeset)[[:space:]]+(.*)$ ]]; then
    take_assignments "${BASH_REMATCH[2]}"
    continue
  fi
  read -ra tok <<< "$seg"
  (( ${#tok[@]} )) || continue

  # Skip env/command wrappers.
  while (( ${#tok[@]} )) && [[ ${tok[0]} == env || ${tok[0]} == sudo || ${tok[0]} == command ]]; do
    [[ ${tok[0]} == sudo ]] && plain=0
    tok=("${tok[@]:1}")
    while (( ${#tok[@]} )) && [[ ${tok[0]} =~ ^[A-Za-z_][A-Za-z0-9_]*= ]]; do
      record_assignments "${tok[0]}"; tok=("${tok[@]:1}")
    done
  done
  (( ${#tok[@]} )) || continue

  # `(cd x && rm y)` and `{ rm y; }`: look past the grouping character. A cd
  # inside a subshell doesn't move the base for the segments after it.
  subshell=0
  while [[ ${tok[0]} == '('* || ${tok[0]} == '{' ]]; do
    [[ ${tok[0]} == '('* ]] && subshell=1
    if [[ ${tok[0]} == '{' || ${tok[0]} == '(' ]]; then tok=("${tok[@]:1}"); else tok[0]=${tok[0]#(}; fi
    (( ${#tok[@]} )) || break
  done
  (( ${#tok[@]} )) || continue

  case $(basename -- "${tok[0]}") in
    export|local|declare|readonly|typeset)
      record_assignments "${tok[@]:1}"
      ;;
    cd|pushd)
      dest=${tok[1]:-$HOME}
      if (( ! subshell )) && [[ $dest != -* ]] && d=$(resolve "$dest"); then base=$d; fi
      ;;
    rm)
      recursive=0 targets=() redir=0
      for t in "${tok[@]:1}"; do
        if (( redir )); then redir=0; continue; fi
        [[ $t =~ ^[0-9]*[\&]?[\<\>] ]] && { [[ $t =~ [\<\>]$ ]] && redir=1; continue; }
        case $t in
          --recursive|--dir) recursive=1 ;;
          --*) ;;
          -*) [[ ${t:1} == *[rR]* ]] && recursive=1 ;;
          *) targets+=("$t") ;;
        esac
      done
      (( ${#targets[@]} )) && check_targets "$recursive" "rm" "${targets[@]}"
      ;;
    rmdir)
      # Only removes empty directories, so nothing to guard; just judge scratch.
      for t in "${tok[@]:1}"; do
        [[ $t == -* ]] && continue
        while IFS= read -r x; do note_scratch "$(classify_target "$x")" "$t" "$x"; done < <(loop_expand "$t")
      done
      ;;
    mkdir|popd|true|:|echo|done|fi|'}'|test|'[')
      ;;
    find)
      plain=0
      [[ $seg == *-delete* || $seg == *-exec*rm* || $seg == *-execdir*rm* ]] || continue
      targets=()
      for t in "${tok[@]:1}"; do
        [[ $t == -* ]] && break
        targets+=("$t")
      done
      (( ${#targets[@]} )) && check_targets 1 "find -delete" "${targets[@]}"
      ;;
    shred)
      plain=0
      targets=()
      for t in "${tok[@]:1}"; do [[ $t == -* ]] || targets+=("$t"); done
      (( ${#targets[@]} )) && check_targets 1 "shred" "${targets[@]}"
      ;;
    *)
      plain=0
      ;;
  esac
done < <(printf '%s' "$cmd" | strip_heredocs | while IFS= read -r l; do split_segments "$l"; done)

# A heredoc body was skipped above, so its command can't be judged plain.
[[ $cmd == *'<<'* ]] && plain=0

if (( plain && deletes )); then
  note_allowed "auto-approved scratch delete"
  allow "rm-guard: every delete target is inside scratch space"
fi

exit 0
