#!/usr/bin/env python3
"""PreToolUse on bash: run Claude Code's shell guards against Muse's commands.

The guards (~/.claude/hooks) read Claude's payload (tool_name "Bash") and
block by exiting 2 or by printing a permissionDecision. Muse names the tool
"bash", runs the command in tool_input.workdir and blocks only through
hookSpecificOutput.permissionDecision, so this translates both ways. Guards
that are missing or crash fail open, as they do under Claude.

When no guard objects, shell_policy decides: allow, so the call skips Muse's
approval prompt, or ask for shared-state, cloud and secret operations. If the
policy itself crashes, the hook makes no decision and Muse's own approval
profile applies.
"""

import json
import os
import subprocess
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

GUARDS = [
    ["python3", "~/.claude/hooks/block-dangerous-bash.py"],
    ["bash", "~/.claude/hooks/rm-guard.sh"],
]


def decide(decision, reason, updated_input=None):
    out = {
        "hookEventName": "PreToolUse",
        "permissionDecision": decision,
        "permissionDecisionReason": reason or "blocked by a shell guard",
    }
    # Muse rejects an allow without updatedInput and falls back to its own review.
    if decision == "allow":
        out["updatedInput"] = updated_input
    json.dump({"hookSpecificOutput": out}, sys.stdout)
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
    try:
        import shell_policy
        reason = shell_policy.classify(tool_input.get("command") or "")
    except Exception as exc:
        print(f"shell_guard: policy error, leaving the decision to Muse: {exc}", file=sys.stderr)
        return
    if reason:
        decide("ask", reason)
    decide("allow", "no shared-state, cloud or secret operation", tool_input)


if __name__ == "__main__":
    main()
