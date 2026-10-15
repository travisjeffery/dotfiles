#!/usr/bin/env python3
"""SessionStart: load `bd prime` (the beads workflow and memories) as context.

Muse doesn't run Claude Code's settings.json hooks, so this is the same
`bd prime` Claude and Codex get from theirs. Muse runs hooks with a scrubbed
environment, so BEADS_DIR, which points bd at the shared database from any
directory, is recovered from the Muse process that started the hook. Fails
open: no bd, no database or an error adds nothing.

Muse kills a hook whose stdout passes 16 KiB, and the full output (every
memory) is far larger, so memories are capped to fit; bd's banner says how
many were left out and how to browse them.
"""

import json
import os
import subprocess
import sys


def ancestor_env(name):
    """name from the nearest ancestor process that has it (Linux /proc)."""
    pid = os.getppid()
    for _ in range(8):
        try:
            with open(f"/proc/{pid}/environ", "rb") as f:
                for kv in f.read().split(b"\0"):
                    k, _, v = kv.partition(b"=")
                    if k.decode() == name and v:
                        return v.decode()
            with open(f"/proc/{pid}/stat") as f:
                pid = int(f.read().rsplit(")", 1)[1].split()[1])
        except (OSError, ValueError, IndexError):
            return None
        if pid <= 1:
            return None
    return None


env = dict(os.environ)
if not env.get("BEADS_DIR") and (d := ancestor_env("BEADS_DIR")):
    env["BEADS_DIR"] = d
LIMIT = 16384 - 512


def context(memory_chars):
    try:
        r = subprocess.run(["bd", "prime", "--max-memory-chars", str(memory_chars)],
                           capture_output=True, text=True, timeout=25, env=env)
    except Exception:
        return None
    if not r.stdout.strip():
        return None
    return json.dumps({"hookSpecificOutput": {"hookEventName": "SessionStart", "additionalContext": r.stdout}},
                      ensure_ascii=False)


budget = 10000
while (out := context(budget)) is not None and len(out.encode()) > LIMIT and budget > 500:
    budget //= 2
if out is not None and len(out.encode()) <= LIMIT:
    sys.stdout.write(out)
