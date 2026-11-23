#!/usr/bin/env python3
"""Antigravity (agy) PreToolUse hook adapter for run_command.

Delegates to the shared Claude Code shell guards (`block-dangerous-bash.py` and
`rm-guard.sh`) as a single source of truth so denylists and rm resolution rules
never drift.

Antigravity's PreToolUse contract (verified on agy 1.5.15):
- stdin JSON carries `toolCall.name` ("run_command"), `toolCall.args.CommandLine`,
  optional `toolCall.args.Cwd`, `cwd`, and `workspacePaths`.
- stdout MUST be a JSON object containing `decision` with exit code 0:
  - "allow": pass through to the next hook or default policy
  - "auto_approve": skip default approval prompt (used for scratch-only deletes)
  - "force_ask": always prompt interactively, ignoring cached permissions
  - "ask": prompt interactively, respecting cached permissions
  - "deny": hard-block immediately (even under --dangerously-skip-permissions)
- Empty stdout, `{}` without `decision`, or a non-zero exit code fails/blocks
  the tool call in agy. To fail open on internal errors (matching the shared
  guards), every error path emits `{"decision": "allow"}` and exits 0.

Environment variables:
- AGY_GUARD_HOOKS_DIR: override directory containing block-dangerous-bash.py
  and rm-guard.sh (primarily for tests).
- AGY_GUARD_ASK_DECISION: map guard "ask" verdicts to "force_ask" (default),
  "ask", or "deny" (useful for headless sessions with --dangerously-skip-permissions).
"""

import argparse
import datetime
import json
import os
import shlex
import shutil
import subprocess
import sys
import tempfile

HOOK_GROUP_NAME = "run-command-guard"
VALID_ASK_DECISIONS = ("force_ask", "ask", "deny")


def emit(decision, reason=None):
    out = {"decision": decision}
    if reason:
        out["reason"] = reason
    json.dump(out, sys.stdout)
    sys.stdout.write("\n")
    sys.exit(0)


def guard_search_dirs():
    override = os.environ.get("AGY_GUARD_HOOKS_DIR")
    if override:
        return [os.path.expanduser(override)]
    repo_hooks = os.path.normpath(
        os.path.join(os.path.dirname(os.path.realpath(__file__)), "..", "..", "..", ".claude", "hooks")
    )
    return [os.path.expanduser("~/.claude/hooks"), repo_hooks]


def find_guard(filename):
    for directory in guard_search_dirs():
        candidate = os.path.join(directory, filename)
        if os.path.isfile(candidate):
            return candidate
    return None


def resolve_ask_decision():
    val = os.environ.get("AGY_GUARD_ASK_DECISION", "force_ask").strip()
    return val if val in VALID_ASK_DECISIONS else "force_ask"


def extract_context(payload):
    """Extract (tool_name, command, session_cwd, args_cwd, project) from an agy PreToolUse payload."""
    tool_call = payload.get("toolCall")
    if not isinstance(tool_call, dict):
        return None, "", "", "", ""
    tool_name = tool_call.get("name")
    args = tool_call.get("args")
    if not isinstance(args, dict):
        args = {}
    command = args.get("CommandLine")
    if not isinstance(command, str):
        command = ""

    workspace_paths = payload.get("workspacePaths")
    first_workspace = ""
    if isinstance(workspace_paths, list):
        for entry in workspace_paths:
            if isinstance(entry, str) and entry.strip():
                first_workspace = entry.strip()
                break

    payload_cwd = payload.get("cwd") if isinstance(payload.get("cwd"), str) else ""
    args_cwd = args.get("Cwd") if isinstance(args.get("Cwd"), str) else ""
    session_cwd = payload_cwd.strip() or first_workspace or os.getcwd()
    project = first_workspace or session_cwd
    args_cwd = args_cwd.strip()
    if args_cwd and not os.path.isabs(args_cwd):
        args_cwd = os.path.normpath(os.path.join(session_cwd, args_cwd))
    return tool_name, command, session_cwd, args_cwd, project


def evaluate(payload):
    """Evaluate an agy PreToolUse payload and return (decision, reason)."""
    if not isinstance(payload, dict):
        return "allow", None

    tool_name, command, session_cwd, args_cwd, project = extract_context(payload)
    if tool_name != "run_command" or not command.strip():
        return "allow", None

    # Keep payload cwd bound to the trusted session/workspace directory so an
    # arbitrary toolCall.args.Cwd (e.g. "/etc") cannot widen ALLOWED_RM_ROOTS.
    # If args.Cwd differs from session_cwd, shift both the subprocess cwd (for
    # block-dangerous-bash.py) and the command's base dir via a leading `cd`
    # (for rm-guard.sh).
    effective_cwd = args_cwd or session_cwd
    guard_command = command
    if args_cwd and os.path.normpath(args_cwd) != os.path.normpath(session_cwd):
        guard_command = f"cd {shlex.quote(args_cwd)}; {command}"

    claude_payload = json.dumps({
        "hook_event_name": "PreToolUse",
        "tool_name": "Bash",
        "tool_input": {"command": guard_command},
        "cwd": session_cwd,
    })
    env = dict(os.environ, CLAUDE_PROJECT_DIR=project)

    guards = [
        (sys.executable, "block-dangerous-bash.py"),
        ("bash", "rm-guard.sh"),
    ]

    ask_reason = None
    auto_approve_reason = None

    # agy executes hooks with process cwd set to the directory containing hooks.json
    # (e.g. ~/.gemini/config), whereas block-dangerous-bash.py resolves relative rm
    # targets against its process cwd. Run the shared guards in the effective cwd.
    run_cwd = (
        effective_cwd
        if os.path.isdir(effective_cwd)
        else (session_cwd if os.path.isdir(session_cwd) else (project if os.path.isdir(project) else None))
    )

    for runner, filename in guards:
        script = find_guard(filename)
        if not script:
            continue
        try:
            result = subprocess.run(
                [runner, script],
                input=claude_payload,
                capture_output=True,
                text=True,
                timeout=10,
                env=env,
                cwd=run_cwd,
            )
        except Exception as exc:
            print(f"run-command-guard: {filename} failed, continuing: {exc}", file=sys.stderr)
            continue

        if result.returncode == 2:
            return "deny", result.stderr.strip() or f"blocked by {filename}"
        if result.returncode != 0:
            # Fail open on unexpected guard crash, matching shared guard contract.
            continue

        stdout = result.stdout.strip()
        if not stdout:
            continue
        try:
            parsed = json.loads(stdout)
        except Exception:
            continue
        if not isinstance(parsed, dict):
            continue
        hso = parsed.get("hookSpecificOutput")
        if not isinstance(hso, dict):
            continue

        decision = hso.get("permissionDecision")
        reason = hso.get("permissionDecisionReason")
        reason = reason.strip() if isinstance(reason, str) and reason.strip() else None

        if decision == "deny":
            return "deny", reason or f"blocked by {filename}"
        if decision == "ask" and ask_reason is None:
            ask_reason = reason or f"{filename} requires confirmation"
        elif decision == "allow" and auto_approve_reason is None:
            auto_approve_reason = reason

    if ask_reason is not None:
        ask_decision = resolve_ask_decision()
        if ask_decision == "deny":
            return "deny", f"{ask_reason} (escalated to deny via AGY_GUARD_ASK_DECISION=deny)"
        return ask_decision, ask_reason

    if auto_approve_reason is not None:
        return "auto_approve", auto_approve_reason

    return "allow", None


def write_json_atomic(path, data, mode=0o644):
    resolved = os.path.realpath(path) if os.path.islink(path) else path
    parent = os.path.dirname(os.path.abspath(resolved))
    os.makedirs(parent, mode=0o700, exist_ok=True)
    content = (json.dumps(data, indent=2, sort_keys=True) + "\n").encode("utf-8")
    fd, tmp_path = tempfile.mkstemp(prefix=".hooks.json.", suffix=".tmp", dir=parent)
    try:
        os.fchmod(fd, mode)
        with os.fdopen(fd, "wb") as f:
            f.write(content)
            f.flush()
            os.fsync(f.fileno())
        os.chmod(tmp_path, mode)
        os.replace(tmp_path, resolved)
    except Exception:
        try:
            os.unlink(tmp_path)
        except OSError:
            pass
        raise


def install_hook(hooks_file, hook_command=None, dry_run=False, backup=True):
    """Idempotently wire run-command-guard into agy's hooks.json, preserving all other groups."""
    hooks_file = os.path.expanduser(hooks_file)
    existing_raw = None
    existing_mode = 0o644
    data = {}

    if os.path.exists(hooks_file):
        st = os.stat(hooks_file)
        existing_mode = st.st_mode & 0o777
        with open(hooks_file, "r", encoding="utf-8") as f:
            existing_raw = f.read()
        stripped = existing_raw.lstrip("\ufeff").strip()
        if stripped:
            parsed = json.loads(stripped)
            if not isinstance(parsed, dict):
                raise ValueError(f"{hooks_file} must contain a JSON object")
            data = parsed

    if hook_command is None:
        script_path = os.path.abspath(__file__)
        hook_command = f"python3 {shlex.quote(script_path)}"

    group = {
        "enabled": True,
        "PreToolUse": [
            {
                "matcher": "run_command",
                "hooks": [
                    {
                        "type": "command",
                        "command": hook_command,
                        "timeout": 15,
                    }
                ],
            }
        ],
    }

    updated = dict(data)
    updated[HOOK_GROUP_NAME] = group
    if data.get(HOOK_GROUP_NAME) == group and existing_raw is not None:
        return {"changed": False, "backup": None, "groups": sorted(updated.keys())}

    backup_path = None
    if not dry_run:
        if backup and existing_raw is not None:
            ts = datetime.datetime.now(datetime.timezone.utc).strftime("%Y%m%dT%H%M%SZ")
            backup_path = f"{hooks_file}.bak-{ts}"
            suffix = 1
            while os.path.exists(backup_path):
                backup_path = f"{hooks_file}.bak-{ts}-{suffix}"
                suffix += 1
            shutil.copy2(hooks_file, backup_path)
        write_json_atomic(hooks_file, updated, mode=existing_mode)

    return {"changed": True, "backup": backup_path, "groups": sorted(updated.keys())}


def run_install_cli(argv):
    parser = argparse.ArgumentParser(
        description="Install run-command-guard into Antigravity hooks.json while preserving existing hooks."
    )
    parser.add_argument("--install", action="store_true", help="Wire this hook into hooks.json")
    parser.add_argument(
        "--hooks-file",
        default="~/.gemini/config/hooks.json",
        help="Path to agy hooks.json (default: ~/.gemini/config/hooks.json)",
    )
    parser.add_argument(
        "--hook-command",
        default=None,
        help="Override command written into hooks.json (default: python3 '<this script>')",
    )
    parser.add_argument("--dry-run", action="store_true", help="Show result without modifying hooks.json")
    parser.add_argument("--no-backup", action="store_true", help="Skip timestamped backup before overwrite")
    args = parser.parse_args(argv)
    if not args.install:
        parser.print_help(sys.stderr)
        return 2
    try:
        res = install_hook(
            args.hooks_file,
            hook_command=args.hook_command,
            dry_run=args.dry_run,
            backup=not args.no_backup,
        )
    except Exception as exc:
        print(f"error: {exc}", file=sys.stderr)
        return 1
    status = "would-update" if (args.dry_run and res["changed"]) else ("updated" if res["changed"] else "unchanged")
    msg = f"{HOOK_GROUP_NAME}: {status} ({args.hooks_file}); groups={','.join(res['groups'])}"
    if res["backup"]:
        msg += f"; backup={res['backup']}"
    print(msg)
    return 0


def main():
    if len(sys.argv) > 1:
        sys.exit(run_install_cli(sys.argv[1:]))

    try:
        payload = json.load(sys.stdin)
    except Exception:
        emit("allow")

    try:
        decision, reason = evaluate(payload)
    except Exception as exc:
        print(f"run-command-guard: internal error, allowing: {exc}", file=sys.stderr)
        emit("allow")

    emit(decision, reason)


if __name__ == "__main__":
    main()
