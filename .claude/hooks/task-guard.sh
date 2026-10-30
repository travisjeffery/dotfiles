#!/usr/bin/env bash
# task-guard.sh — auto-allow subagent types that are read-only/narrowly
# scoped by design; ask for anything with broader tool access, except inside
# worker worktrees, where workers run unattended and a prompt stalls
# the thread until someone answers it.

input=$(cat)
subagent_type=$(echo "$input" | jq -r '.tool_input.subagent_type // empty')
cwd=$(echo "$input" | jq -r '.cwd // empty')

case "$subagent_type" in
  Explore|Plan\
  |feature-dev:code-explorer|feature-dev:code-architect\
  |hookify:conversation-analyzer\
  |codebase-analyzer|code-search|docs-lookup\
  |claude-code-guide)
    exit 0 ;;  # known-safe, read-only-by-design — fall through, no prompt
esac

# Claude Code enforces an agent definition's `tools:` list, so an agent whose
# list holds only these can't write, run commands, or delegate further.
# KillShell/BashOutput only reach shells the agent has no way to start.
READ_ONLY_TOOLS=" Read Grep Glob LS NotebookRead WebFetch WebSearch TodoWrite KillShell BashOutput "

# Prints the `tools:` entries of an agent definition one per line, or
# nothing when the field is absent (the agent then inherits every tool).
agent_tools() {
  awk '
    NR == 1 && $0 != "---" { exit }
    NR > 1 && $0 == "---" { exit }
    in_list && /^[[:space:]]*-[[:space:]]*/ { sub(/^[[:space:]]*-[[:space:]]*/, ""); print; next }
    in_list { exit }
    /^tools:[[:space:]]*$/ { in_list = 1; next }
    /^tools:/ { sub(/^tools:[[:space:]]*/, ""); gsub(/[][,"'"'"']/, " "); n = split($0, t, /[[:space:]]+/); for (i = 1; i <= n; i++) if (t[i] != "") print t[i]; exit }
  ' "$1" | tr -d "\"'\r"
}

read_only_definition() {
  local tools tool
  tools=$(agent_tools "$1")
  [[ -n $tools ]] || return 1
  while IFS= read -r tool; do
    [[ $READ_ONLY_TOOLS == *" $tool "* ]] || return 1
  done <<< "$tools"
}

# Every definition Claude Code could resolve the type to: project and user
# agents for a bare name, each installed copy of the plugin for plugin:name.
agent_definitions() {
  local type=$1 plugin name dir
  if [[ $type == *:* ]]; then
    plugin=${type%%:*} name=${type#*:}
    name=${name//./\\.}
    {
      jq -r --arg p "$plugin" '.plugins | to_entries[] | select(.key | startswith($p + "@")) | .value[].installPath' \
        "$HOME/.claude/plugins/installed_plugins.json" 2>/dev/null
      for dir in "$HOME"/.claude/plugins/synced/*/"$plugin" "$HOME"/.claude/plugins/synced/*/"$plugin"~*; do
        [[ -d $dir ]] && echo "$dir"
      done
    } | while IFS= read -r dir; do
      grep -lxE "name: *[\"']?$name[\"']? *" "$dir"/agents/*.md 2>/dev/null
    done
  else
    for dir in "${CLAUDE_PROJECT_DIR:-$cwd}" "$HOME"; do
      [[ -n $dir && -f $dir/.claude/agents/$type.md ]] && echo "$dir/.claude/agents/$type.md"
    done
  fi
}

if [[ $subagent_type =~ ^[A-Za-z0-9._~-]+(:[A-Za-z0-9._~-]+)?$ ]]; then
  definitions=$(agent_definitions "$subagent_type")
  if [[ -n $definitions ]]; then
    all_read_only=1
    while IFS= read -r def; do
      read_only_definition "$def" || { all_read_only=0; break; }
    done <<< "$definitions"
    (( all_read_only )) && exit 0
  fi
fi

# Workers live in /data/worktrees/<repo>/<branch> (~/worktrees before the move
# to the /data NVMe). A subagent inherits the worker's permission mode and the
# auto-mode classifier, so its tool calls are still checked one by one; this
# only skips the "may I delegate?" prompt.
case "$cwd" in
  /data/worktrees/*/*|"$HOME"/worktrees/*/*)
    exit 0 ;;
esac

# general-purpose, Bash-capable, or unknown/custom types get a confirmation
echo '{"hookSpecificOutput": {"hookEventName": "PreToolUse", "permissionDecision": "ask", "permissionDecisionReason": "Subagent type '"$subagent_type"' has broad tool access — confirm before delegating."}}'
