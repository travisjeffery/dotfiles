#!/usr/bin/env python3
"""PreToolUse on bash: run Claude Code's shell guards against Muse's commands.

The guards (~/.claude/hooks) read Claude's payload (tool_name "Bash") and
block by exiting 2 or by printing a permissionDecision. Muse names the tool
"bash", runs the command in tool_input.workdir and blocks only through
hookSpecificOutput.permissionDecision, so this translates both ways. Guards
that are missing or crash fail open, as they do under Claude.
"""

import json
import os
import re
import subprocess
import sys

# claude-slack is read-only by construction (see its script), so a bare call
# with one quoted request skips the approval prompt. Double-quoted requests may
# not contain $ or backticks, which bash would still expand.
CLAUDE_SLACK = re.compile(
    r"""\s*claude-slack(?:\s+--json)?(?:\s+--model\s+[A-Za-z0-9._-]+)?(?:\s+--json)?"""
    r"""\s+(?:'[^']*'|"(?:[^"\\$`]|\\["\\])*")\s*"""
)

GUARDS = [
    ["python3", "~/.claude/hooks/block-dangerous-bash.py"],
    ["bash", "~/.claude/hooks/rm-guard.sh"],
]


def decide(decision, reason):
    json.dump({"hookSpecificOutput": {
        "hookEventName": "PreToolUse",
        "permissionDecision": decision,
        "permissionDecisionReason": reason or "blocked by a shell guard",
    }}, sys.stdout)
    sys.exit(0)


def main():
    try:
        payload = json.load(sys.stdin)
    except Exception:
        return
    if payload.get("tool_name") != "bash":
        return
    tool_input = payload.get("tool_input") or {}
    cwd = tool_input.get("workdir") or payload.get("cwd") or os.getcwd()
    claude = dict(payload, tool_name="Bash", cwd=cwd, hook_event_name="PreToolUse")
    env = dict(os.environ, CLAUDE_PROJECT_DIR=payload.get("cwd") or cwd)
    ask = None
    for argv in GUARDS:
        argv = [os.path.expanduser(a) for a in argv]
        if not os.path.isfile(argv[-1]):
            continue
        try:
            r = subprocess.run(argv, input=json.dumps(claude), capture_output=True, text=True, timeout=10, env=env)
        except Exception:
            continue
        if r.returncode == 2:
            decide("deny", r.stderr.strip())
        try:
            out = json.loads(r.stdout) if r.stdout.strip() else {}
            hso = out.get("hookSpecificOutput") or {}
        except Exception:
            continue
        d = hso.get("permissionDecision")
        if d == "deny":
            decide("deny", hso.get("permissionDecisionReason"))
        if d == "ask" and ask is None:
            ask = hso.get("permissionDecisionReason")
    if ask is not None:
        decide("ask", ask)
    if CLAUDE_SLACK.fullmatch(tool_input.get("command") or ""):
        decide("allow", "read-only claude-slack lookup")


if __name__ == "__main__":
    main()
