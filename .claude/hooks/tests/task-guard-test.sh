#!/usr/bin/env bash
# Pipes Agent PreToolUse payloads through task-guard.sh and checks each verdict.
# Usage: task-guard-test.sh [path/to/task-guard.sh]

set -uo pipefail

HOOK=${1:-$(dirname -- "$(realpath -- "$0")")/../task-guard.sh}
REAL_HOME=$HOME
work=$(mktemp -d -p "$HOME/.cache" task-guard-test.XXXXXX)
trap 'rm -rf -- "$work"' EXIT

pass=0 fail=0

# verdict TYPE CWD [HOME] -> allow | ask | error(RC)
verdict() {
  local out rc
  out=$(jq -nc --arg t "$1" --arg d "$2" '{tool_name:"Agent",tool_input:{subagent_type:$t,prompt:"x"},cwd:$d}' |
    env -u CLAUDE_PROJECT_DIR HOME="${3:-$fake}" bash "$HOOK" 2>/dev/null)
  rc=$?
  if (( rc != 0 )); then echo "error($rc)"
  elif [[ $out == *'"ask"'* ]]; then echo ask
  else echo allow
  fi
}

expect() {
  local want=$1 name=$2 got
  got=$(verdict "${@:3}")
  if [[ $got == "$want" ]]; then printf 'ok   %s: %s\n' "$name" "$got"; (( pass++ ))
  else printf 'FAIL %s: want %s, got %s\n' "$name" "$want" "$got"; (( fail++ ))
  fi
}

# agent FILE NAME [TOOLS_FRONTMATTER_LINES...]
agent() {
  local file=$1 name=$2; shift 2
  mkdir -p "$(dirname "$file")"
  { echo ---; echo "name: $name"; echo "description: test"; printf '%s\n' "$@"; echo ---; echo body; } > "$file"
}

fake=$work/home
proj=$work/project
cache=$fake/.claude/plugins/cache/mkt
synced=$fake/.claude/plugins/synced/org
mkdir -p "$proj" "$fake/.claude/plugins"
jq -n --arg r "$cache/revtool/1.0.0" --arg b "$cache/broad/1.0.0" \
  '{plugins:{"revtool@mkt":[{installPath:$r}],"broad@mkt":[{installPath:$b}]}}' \
  > "$fake/.claude/plugins/installed_plugins.json"

agent "$proj/.claude/agents/ro-reviewer.md" ro-reviewer 'tools: Read, Grep, Glob'
agent "$proj/.claude/agents/bash-reviewer.md" bash-reviewer 'tools: Read, Grep, Glob, Bash'
agent "$proj/.claude/agents/inherits.md" inherits
agent "$proj/.claude/agents/yaml-list.md" yaml-list 'tools:' '  - Read' '  - Grep'
agent "$proj/.claude/agents/yaml-list-write.md" yaml-list-write 'tools:' '  - Read' '  - Edit'
agent "$proj/.claude/agents/flow.md" flow 'tools: [Read, "Grep"]'
agent "$proj/.claude/agents/star.md" star 'tools: "*"'
agent "$proj/.claude/agents/nests.md" nests 'tools: Read, Agent'
agent "$proj/.claude/agents/mcp.md" mcp 'tools: Read, mcp__github__create_pull_request'
agent "$fake/.claude/agents/user-ro.md" user-ro 'tools: Read, LS'
agent "$cache/revtool/1.0.0/agents/reviewer.md" reviewer 'tools: Glob, Grep, LS, Read, NotebookRead, WebFetch, TodoWrite, WebSearch, KillShell, BashOutput'
agent "$cache/revtool/1.0.0/agents/renamed-file.md" by-name 'tools: Read'
agent "$cache/broad/1.0.0/agents/reviewer.md" reviewer
agent "$synced/lens/agents/reviewer.md" reviewer 'tools: Read, Grep, Glob'
agent "$synced/split/agents/reviewer.md" reviewer 'tools: Read'
agent "$synced/split~g2/agents/reviewer.md" reviewer 'tools: Read, Write'
agent "$synced/dotty/agents/aXb.md" aXb 'tools: Read, Write'
# Frontmatter must open on line one; a `tools:` line in the body counts for nothing.
printf '%s\n' 'intro' '---' 'name: late' 'tools: Read' '---' > "$proj/.claude/agents/late.md"
printf '%s\n' '---' 'name: body-tools' '---' 'tools: Read' > "$proj/.claude/agents/body-tools.md"

elsewhere=$work/elsewhere
mkdir -p "$elsewhere"

expect allow "built-in Explore" Explore "$elsewhere"
expect ask   "general-purpose" general-purpose "$elsewhere"
expect ask   "no subagent_type (general-purpose)" "" "$elsewhere"
expect ask   "unknown type" no-such-agent "$elsewhere"

expect allow "project agent, read-only tools" ro-reviewer "$proj"
expect ask   "project agent with Bash" bash-reviewer "$proj"
expect ask   "project agent inheriting all tools" inherits "$proj"
expect allow "project agent, YAML list of read tools" yaml-list "$proj"
expect ask   "project agent, YAML list with Edit" yaml-list-write "$proj"
expect allow "project agent, flow list" flow "$proj"
expect ask   "project agent, tools *" star "$proj"
expect ask   "project agent that can delegate" nests "$proj"
expect ask   "project agent with an MCP write tool" mcp "$proj"
expect ask   "frontmatter not on line one" late "$proj"
expect ask   "tools: only in the body" body-tools "$proj"
expect ask   "project agent from another cwd" ro-reviewer "$elsewhere"
expect allow "user agent, read-only" user-ro "$elsewhere"

expect allow "installed plugin, read-only" revtool:reviewer "$elsewhere"
expect allow "installed plugin, matched by name: not filename" revtool:by-name "$elsewhere"
expect ask   "installed plugin inheriting all tools" broad:reviewer "$elsewhere"
expect allow "synced plugin, read-only" lens:reviewer "$elsewhere"
expect ask   "synced plugin, one copy can write" split:reviewer "$elsewhere"
expect ask   "dot in name is not a wildcard" dotty:a.b "$elsewhere"
expect ask   "plugin agent missing" revtool:nope "$elsewhere"
expect ask   "path traversal in type" "../../etc/x" "$proj"

expect allow "write-capable agent inside a ~/worktrees worker" general-purpose "$fake/worktrees/backend/branch"
expect allow "write-capable agent inside a /data/worktrees worker" general-purpose /data/worktrees/backend/tj-branch
expect allow "nested dir of a /data/worktrees worker" general-purpose /data/worktrees/backend/tj-branch/deployer
expect allow "unknown custom agent inside a /data/worktrees worker" no-such-agent /data/worktrees/backend/tj-branch
expect ask   "/data/worktrees repo root is not a worker" general-purpose /data/worktrees/backend
expect ask   "/data/worktrees itself is not a worker" general-purpose /data/worktrees
expect ask   "~/worktrees repo root is not a worker" general-purpose "$fake/worktrees/backend"
expect ask   "other /data path" general-purpose /data/src/backend/x
expect ask   "lookalike /data/worktrees-old" general-purpose /data/worktrees-old/backend/branch
expect ask   "main checkout under ~/src" general-purpose "$fake/src/backend"

# The definitions on this machine, so plugin updates that widen a reviewer show up here.
if [[ -f $REAL_HOME/.claude/plugins/installed_plugins.json ]]; then
  outside=$work/outside
  mkdir -p "$outside"
  expect allow "live feature-dev:code-reviewer" feature-dev:code-reviewer "$outside" "$REAL_HOME"
  expect ask   "live pr-review-toolkit:code-reviewer (inherits all tools)" pr-review-toolkit:code-reviewer "$outside" "$REAL_HOME"
  expect ask   "live general-purpose" general-purpose "$outside" "$REAL_HOME"
  if compgen -G "$REAL_HOME/.claude/plugins/synced/*/osc-review/agents/code-reviewer.md" > /dev/null; then
    expect allow "live osc-review:code-reviewer" osc-review:code-reviewer "$outside" "$REAL_HOME"
  fi
fi

printf '\n%d passed, %d failed\n' "$pass" "$fail"
(( fail == 0 ))
