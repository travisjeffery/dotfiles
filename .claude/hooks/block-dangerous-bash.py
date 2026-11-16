#!/usr/bin/env python3
"""PreToolUse guard for Bash: blocks a small denylist of irreversible commands.

Paired with a bare "Bash" allow rule, so everything else runs unprompted.
A code-2 exit blocks the call before permission rules are evaluated, which is
why this can veto commands that the allow rule would otherwise let through.

Internal errors deliberately fail OPEN (exit 0): this guards against accidents,
not an adversary, and a crashing hook that blocked every command would wedge
the session.
"""

import json
import os
import re
import shlex
import sys

# rm -rf is permitted only when every target stays inside one of these roots.
# $CWD and $PROJECT are substituted from the hook payload at runtime.
ALLOWED_RM_ROOTS = ["$PROJECT", "$CWD", "/tmp", "/var/tmp", os.path.expanduser("~/.cache")]


def strip_heredocs(command):
    """Drop heredoc bodies: they are data, not commands.

    Without this, writing a commit message or doc that merely mentions a
    blocked command trips the guard.
    """
    out, lines, i = [], command.split("\n"), 0
    while i < len(lines):
        line = lines[i]
        out.append(line)
        for m in re.finditer(r"<<-?\s*['\"]?([A-Za-z_][A-Za-z0-9_]*)['\"]?", line):
            delim = m.group(1)
            i += 1
            while i < len(lines) and lines[i].strip() != delim:
                i += 1
            break
        i += 1
    return "\n".join(out)


def segments(command):
    """Split a compound command on shell operators, ignoring quoted text.

    `cd /tmp && rm -rf /` must be checked piecewise, but the `&&` inside
    `echo "a && b"` is data and must not split.
    """
    parts, buf, quote, esc = [], [], None, False
    i = 0
    while i < len(command):
        c = command[i]
        if esc:
            buf.append(c); esc = False; i += 1; continue
        if c == "\\":
            buf.append(c); esc = True; i += 1; continue
        if quote:
            buf.append(c)
            if c == quote:
                quote = None
            i += 1
            continue
        if c in "\"'":
            quote = c; buf.append(c); i += 1; continue
        if command[i:i + 2] in ("&&", "||"):
            parts.append("".join(buf)); buf = []; i += 2; continue
        if c in ";|\n":
            parts.append("".join(buf)); buf = []; i += 1; continue
        buf.append(c); i += 1
    parts.append("".join(buf))
    return [s for s in parts if s.strip()]


def tokens(segment):
    try:
        return shlex.split(segment)
    except ValueError:
        return segment.split()


ASSIGNMENT = re.compile(r"^[A-Za-z_][A-Za-z0-9_]*=")
# Value-taking options of the wrappers unwrap() steps over.
WRAPPER_VALUE = {
    "sudo": {"-u", "-g", "-U", "-C", "-D", "-h", "-p", "-r", "-t", "-T"},
    "env": {"-u", "--unset", "-C", "--chdir"},
    "timeout": {"-s", "--signal", "-k", "--kill-after"},
    "nice": {"-n"},
}


def skip_options(args, value_flags):
    i = 0
    while i < len(args) and args[i].startswith("-"):
        i += 2 if args[i] in value_flags else 1
    return args[i:]


def unwrap(tok):
    """Strip env-var prefixes and wrappers, so `AWS_PROFILE=x sudo aws ...` reads as `aws ...`."""
    while tok:
        if ASSIGNMENT.match(tok[0]):
            tok = tok[1:]
            continue
        name = os.path.basename(tok[0])
        if name in ("sudo", "env", "nice", "nohup", "time", "command", "exec"):
            tok = skip_options(tok[1:], WRAPPER_VALUE.get(name, ()))
        elif name == "timeout":
            tok = skip_options(tok[1:], WRAPPER_VALUE["timeout"])[1:]
        elif name == "aws-vault" and "--" in tok:
            tok = tok[tok.index("--") + 1:]
        else:
            return tok
    return tok


def positionals(args, value_flags):
    """Non-option words, skipping the value of each option in value_flags."""
    out, skip = [], False
    for a in args:
        if skip:
            skip = False
        elif a.startswith("-"):
            skip = a in value_flags
        else:
            out.append(a)
    return out


AWS_GLOBAL_VALUE = {"--profile", "--region", "--output", "--query", "--endpoint-url", "--color",
                    "--ca-bundle", "--cli-read-timeout", "--cli-connect-timeout", "--cli-binary-format"}
# Read-only verbs are never blocked, whatever words follow them.
AWS_READ = re.compile(r"^(describe|list|get|head)-")
# Destroy-shaped operations whose names carry no delete or terminate word.
AWS_DESTROY = {
    ("s3", "rm"), ("s3", "rb"),
    ("sqs", "purge-queue"),
    ("ec2", "deregister-image"), ("ec2", "release-address"), ("ec2", "release-hosts"),
    ("kms", "schedule-key-deletion"),
    ("account", "disable-region"),
    ("organizations", "close-account"), ("organizations", "leave-organization"),
}


def aws_destroy(tok):
    """Return "service operation" when an aws call deletes or destroys, else None."""
    pos = positionals(tok[1:], AWS_GLOBAL_VALUE)
    if len(pos) < 2:
        return None
    service, op = pos[0], pos[1]
    if AWS_READ.match(op):
        return None
    # ec2 checks permissions without acting under --dry-run; s3 spells it --dryrun.
    if service == "ec2" and "--dry-run" in tok and "--no-dry-run" not in tok:
        return None
    if service == "s3" and "--dryrun" in tok:
        return None
    words = op.split("-")
    if (
        "delete" in words
        or "terminate" in words
        or (service, op) in AWS_DESTROY
        or (service, op) == ("s3", "sync") and "--delete" in tok
    ):
        return f"{service} {op}"
    return None


HELM_GLOBAL_VALUE = {
    "-n", "--namespace", "--kube-context", "--kubeconfig", "--burst-limit", "--qps", "--kube-apiserver",
    "--kube-as-group", "--kube-as-user", "--kube-ca-file", "--kube-tls-server-name", "--kube-token",
    "--registry-config", "--repository-cache", "--repository-config", "--content-cache",
}
HELM_UNINSTALL = {"uninstall", "delete", "del", "un"}


def is_helm_uninstall(tok):
    pos = positionals(tok[1:], HELM_GLOBAL_VALUE)
    return bool(pos) and pos[0] in HELM_UNINSTALL and "--dry-run" not in tok


def is_rm_rf(tok):
    if not tok or os.path.basename(tok[0]) != "rm":
        return False
    flags = "".join(t.lstrip("-") for t in tok[1:] if t.startswith("-") and not t.startswith("--"))
    long = {t for t in tok[1:] if t.startswith("--")}
    recursive = "r" in flags.lower() or "--recursive" in long
    force = "f" in flags or "--force" in long
    return recursive and force


def rm_targets(tok):
    return [t for t in tok[1:] if not t.startswith("-")]


def outside_allowed(target, roots):
    expanded = os.path.expanduser(os.path.expandvars(target))
    resolved = os.path.realpath(expanded)
    if resolved == "/":
        return True
    return not any(
        resolved == r or resolved.startswith(r.rstrip("/") + "/")
        for r in roots
        if r
    )


def check(command, cwd, project):
    roots = [
        project if r == "$PROJECT" else cwd if r == "$CWD" else r
        for r in ALLOWED_RM_ROOTS
    ]
    roots = [os.path.realpath(r) for r in roots if r]

    for seg in segments(strip_heredocs(command)):
        tok = unwrap(tokens(seg))
        if not tok:
            continue
        joined = " ".join(tok)

        if is_rm_rf(tok):
            for t in rm_targets(tok):
                if outside_allowed(t, roots):
                    return (
                        f"rm -rf targets {t!r}, which resolves outside the allowed roots "
                        f"({', '.join(roots)}). Delete it manually if you meant to."
                    )

        # --force-with-lease is the safe variant: it refuses to clobber refs that
        # moved since the last fetch, so it is deliberately NOT blocked.
        if re.match(r"^git\b", joined) and "push" in tok:
            if any(t == "--force" or (t.startswith("-") and not t.startswith("--") and "f" in t.lstrip("-")) for t in tok):
                return "git push --force rewrites remote history. Use --force-with-lease instead."

        if re.match(r"^kubectl\b", joined) and "delete" in tok:
            return "kubectl delete is blocked. Run it yourself if you are certain."

        if re.match(r"^(terraform|pulumi)\b", joined) and "destroy" in tok:
            return f"{tok[0]} destroy tears down real infrastructure. Run it yourself if intended."

        if os.path.basename(tok[0]) == "aws":
            op = aws_destroy(tok)
            if op:
                return f"aws {op} deletes or destroys AWS resources. Run it yourself if intended."

        if os.path.basename(tok[0]) == "helm" and is_helm_uninstall(tok):
            return "helm uninstall removes a release and every resource it created. Run it yourself if intended."

    return None


def main():
    try:
        payload = json.load(sys.stdin)
    except Exception:
        sys.exit(0)

    if payload.get("tool_name") != "Bash":
        sys.exit(0)

    command = (payload.get("tool_input") or {}).get("command") or ""
    cwd = payload.get("cwd") or os.getcwd()
    project = os.environ.get("CLAUDE_PROJECT_DIR") or cwd

    try:
        reason = check(command, cwd, project)
    except Exception as exc:
        print(f"block-dangerous-bash: internal error, allowing: {exc}", file=sys.stderr)
        sys.exit(0)

    if reason:
        print(f"Blocked by block-dangerous-bash: {reason}", file=sys.stderr)
        sys.exit(2)
    sys.exit(0)


if __name__ == "__main__":
    main()
