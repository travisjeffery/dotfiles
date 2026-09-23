#!/usr/bin/env python3
"""Name this pane's herdr workspace, agent and tab after the bead it just claimed.

Custom hook, deliberately NOT part of herdr-agent-state.sh: that file is managed
by herdr and overwritten whenever the claude integration is reinstalled.

Registered on PostToolUse/Bash in BOTH ~/.claude/settings.json and
~/.codex/hooks.json. Never fails the tool call -- every path exits 0.
"""
import json
import os
import re
import subprocess
import sys

MAX_PROSE = 34

# bd ids look like "backend-4m6x" or "backend-ndsd.3"; herdr agent names allow
# only [a-z][a-z0-9_-]{0,31}, so the dot of a sub-bead becomes a dash.
BEAD_RE = re.compile(r"\b([a-z][a-z0-9]{1,15}-[a-z0-9]{2,8}(?:\.[0-9]+)?)\b")
CLAIM_RE = re.compile(r"\bbd\b.*?(--claim\b|--status[= ]\s*in_progress\b)")


def run(*args):
    try:
        p = subprocess.run(args, capture_output=True, text=True, timeout=5)
        return p.stdout
    except Exception:
        return ""


def main():
    pane = os.environ.get("HERDR_PANE_ID", "")
    if os.environ.get("HERDR_ENV") != "1" or not pane:
        return

    raw = sys.stdin.read()
    try:
        data = json.loads(raw)
    except Exception:
        data = {}

    # Claude Code and Codex both send {tool_input: {command}}, but Codex's tool
    # naming shifts with unified_exec, so fall back to scanning the raw payload
    # rather than silently doing nothing on a shape we did not anticipate.
    ti = data.get("tool_input") or data.get("input") or {}
    cmd = ""
    if isinstance(ti, dict):
        cmd = ti.get("command") or ti.get("cmd") or ""
    if isinstance(cmd, list):
        cmd = " ".join(str(x) for x in cmd)
    if not cmd:
        cmd = raw
    if not CLAIM_RE.search(cmd):
        return
    m = BEAD_RE.search(cmd)
    if not m:
        return
    bead = m.group(1)

    safe = re.sub(r"[^a-z0-9_-]", "-", bead.replace(".", "-").lower())[:32]
    if not safe or not safe[0].isalpha():
        return

    try:
        info = json.loads(run("herdr", "agent", "get", pane))["result"]["agent"]
    except Exception:
        return
    if info.get("name") == safe:
        return

    run("herdr", "agent", "rename", pane, safe)

    prose = ""
    try:
        prose = (json.loads(run("bd", "show", bead, "--json")) or [{}])[0].get("title", "")
    except Exception:
        pass
    prose = " ".join(prose.split())
    if len(prose) > MAX_PROSE:
        cut = prose[:MAX_PROSE].rsplit(" ", 1)[0]
        prose = (cut or prose[:MAX_PROSE]).rstrip(" ,:;-")

    # The agent panel renders "<workspace> . <tab>", so the id goes on the
    # workspace and the tab carries prose alone -- otherwise the id is printed
    # twice and the long half is what gets truncated away.
    ws = info.get("workspace_id")
    if ws:
        try:
            w = json.loads(run("herdr", "workspace", "get", ws))["result"]["workspace"]
        except Exception:
            w = {}
        # Only when this workspace holds nothing but us: a shared one would be
        # labelled with whichever bead happened to be claimed last.
        if w.get("pane_count") in (None, 1) and w.get("tab_count") in (None, 1):
            run("herdr", "workspace", "rename", ws, safe)

    tab = info.get("tab_id")
    if tab and prose:
        run("herdr", "tab", "rename", tab, prose)


if __name__ == "__main__":
    try:
        main()
    except Exception:
        pass
    sys.exit(0)
