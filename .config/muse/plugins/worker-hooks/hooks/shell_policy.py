"""Classify a bash command as "allow" or "ask" for Muse's shell guard.

Everything runs unprompted except commands that change shared or remote state
(cloud resources, clusters, infrastructure, remote branches, registries,
remote databases), read secrets, or message people. Commands are read by
meaning: each segment of a compound command, the inside of $(...), backticks,
bash -c and common wrappers. Heredoc bodies, comments and quoted arguments are
data. The hard denylist stays in the Claude guards, which run first.
"""

import os
import re
import shlex

MAX_DEPTH = 8

KEYWORDS = {"if", "then", "else", "elif", "fi", "do", "done", "while", "until",
            "for", "in", "case", "esac", "!", "{", "}", "function", "select", "coproc"}
ASSIGNMENT = re.compile(r"^[A-Za-z_][A-Za-z0-9_]*(\[[^]]*\])?\+?=")
REDIRECT = re.compile(r"^(\d*|&)(>>?|<<?<?|>&|<&|&>>?|>\|)(.*)$")
LOCAL_HOST = re.compile(r"^(localhost|127(\.\d+){3}|0\.0\.0\.0|\[?::1\]?|[^/]*\.localhost|host\.docker\.internal)$")

SECRET_PATH = re.compile(
    r"(^|[/=@:~])("
    r"\.aws/(credentials|sso/cache|cli/cache)|\.netrc|\.git-credentials|\.config/gh/hosts\.yml"
    r"|\.docker/config\.json|\.pgpass|\.pypirc|\.npmrc|\.ssh/id_[^/]*?(?<!\.pub)$|\.config/op/"
    r"|\.config/muse/auth\.json|\.codex/auth\.json|\.claude/\.credentials\.json"
    r"|\.env(\.(?!example$|sample$|template$)[\w.-]+)?$|[^/.][^/]*\.(key|p12|pfx)$|[^/]*key\.pem$"
    r"|\.config/gcloud/(application_default_credentials\.json|credentials\.db|access_tokens\.db|legacy_credentials)"
    r"|/var/run/secrets/|/etc/shadow"
    r")"
)
SECRET_VAR = re.compile(r"\$\{?[A-Za-z0-9_]*(TOKEN|SECRET|PASSWORD|PASSWD|API_KEY|ACCESS_KEY|PRIVATE_KEY|CREDENTIALS?)S?(?![A-Za-z0-9_])", re.I)
PATH_ONLY = {"git", "ls", "stat", "test", "[", "[[", "file", "chmod", "chown", "touch", "mkdir",
             "rm", "du", "realpath", "readlink", "dirname", "basename", "wc"}


SHELL_STDIN = re.compile(r"(^|[;&|(]\s*)(sudo\s+)?(ba|z|da|k)?sh(\s+-[a-z]+)*\s*$")


def strip_heredocs(command):
    """Drop heredoc bodies, returning (command, bodies a shell will run)."""
    out, scripts, lines, i = [], [], command.split("\n"), 0
    while i < len(lines):
        line = lines[i]
        out.append(line)
        m = re.search(r"<<-?\s*['\"]?([A-Za-z_][A-Za-z0-9_]*)['\"]?", line)
        if m and "<<<" not in line[max(0, m.start() - 1):m.start() + 3]:
            body, i = [], i + 1
            while i < len(lines) and lines[i].strip() != m.group(1):
                body.append(lines[i])
                i += 1
            if SHELL_STDIN.search(line[:m.start()]):
                scripts.append("\n".join(body))
        i += 1
    return "\n".join(out), scripts


def _close(command, i, open_ch="(", close_ch=")"):
    """Index of the bracket closing the one opened just before i."""
    depth, quote = 1, None
    while i < len(command):
        c = command[i]
        if c == "\\" and quote != "'":
            i += 2
            continue
        if quote:
            if c == quote:
                quote = None
        elif c in "\"'":
            quote = c
        elif c == open_ch:
            depth += 1
        elif c == close_ch:
            depth -= 1
            if depth == 0:
                return i
        i += 1
    return len(command)


def split(command):
    """Return (segments, nested commands from $(...), `...`, <(...))."""
    parts, nested, buf, quote = [], [], [], None
    i, n = 0, len(command)

    def flush():
        parts.append("".join(buf))
        buf.clear()

    while i < n:
        c = command[i]
        if c == "\\" and quote != "'":
            buf.append(command[i:i + 2]); i += 2; continue
        if quote == "'":
            buf.append(c)
            if c == "'":
                quote = None
            i += 1
            continue
        if command.startswith("$((", i):
            j = _close(command, i + 3)
            j = j + 1 if command[j + 1:j + 2] == ")" else j
            buf.append("0"); i = j + 1; continue
        if command.startswith("$(", i) or (quote is None and command[i:i + 2] in ("<(", ">(")):
            j = _close(command, i + 2)
            nested.append(command[i + 2:j]); buf.append("_"); i = j + 1; continue
        if c == "`":
            j = command.find("`", i + 1)
            j = n if j < 0 else j
            nested.append(command[i + 1:j]); buf.append("_"); i = j + 1; continue
        if quote == '"':
            buf.append(c)
            if c == '"':
                quote = None
            i += 1
            continue
        if c in "\"'":
            quote = c; buf.append(c); i += 1; continue
        if c == "#" and (not buf or buf[-1].isspace()):
            while i < n and command[i] != "\n":
                i += 1
            continue
        if c == "&" and (command[i + 1:i + 2] == ">" or (buf and buf[-1] in "<>")):
            buf.append(c); i += 1; continue
        if command.startswith(("&&", "||", "|&", ";;"), i):
            flush(); i += 2; continue
        if c in ";|&\n()":
            flush(); i += 1; continue
        buf.append(c); i += 1
    flush()
    return [p for p in parts if p.strip()], nested


def tokens(segment):
    try:
        return shlex.split(segment)
    except ValueError:
        return segment.split()


def strip_noise(tok):
    out, skip = [], False
    for t in tok:
        if skip:
            skip = False
            continue
        m = REDIRECT.match(t)
        if m:
            skip = m.group(3) == ""
            continue
        out.append(t)
    while out and (out[0] in KEYWORDS or ASSIGNMENT.match(out[0])):
        out = out[1:]
    return out


def positionals(args, value_flags=()):
    out, skip = [], False
    for a in args:
        if skip:
            skip = False
        elif a.startswith("-"):
            skip = a in value_flags
        else:
            out.append(a)
    return out


def flag_value(args, *names):
    for i, a in enumerate(args):
        for name in names:
            if a == name and i + 1 < len(args):
                return args[i + 1]
            if name.startswith("--") and a.startswith(name + "="):
                return a[len(name) + 1:]
            if not name.startswith("--") and a.startswith(name) and len(a) > len(name):
                return a[len(name):]
    return None


def has(args, *names):
    return any(a in names or any(a.startswith(n + "=") for n in names if n.startswith("--")) for a in args)


def host_of(url):
    m = re.match(r"^(?:[a-z][a-z0-9+.-]*://)?(?:[^@/]*@)?(\[[^]]*\]|[^:/?#]*)", url)
    return m.group(1) if m else url


def is_local(host):
    return not host or host.startswith("/") or bool(LOCAL_HOST.match(host))


# --- per-tool classifiers: return a reason to ask, or None ---------------------

AWS_GLOBAL_VALUE = {"--profile", "--region", "--output", "--query", "--endpoint-url", "--color",
                    "--ca-bundle", "--cli-read-timeout", "--cli-connect-timeout", "--cli-binary-format"}
S3_VALUE = {"--exclude", "--include", "--acl", "--storage-class", "--content-type", "--cache-control", "--sse",
            "--sse-kms-key-id", "--sse-c", "--sse-c-key", "--grants", "--metadata", "--metadata-directive",
            "--expires", "--content-encoding", "--content-disposition", "--content-language", "--source-region",
            "--page-size", "--expected-size", "--copy-props", "--checksum-algorithm", "--website-redirect"}
AWS_READ = re.compile(r"^(describe|list|get|head|search|lookup|filter|batch-get|batch-describe|scan|query"
                      r"|validate|simulate|preview|wait|help|tail)(-|$)")
AWS_SECRET = {
    ("secretsmanager", "get-secret-value"), ("secretsmanager", "batch-get-secret-value"),
    ("ecr", "get-login-password"), ("ecr", "get-authorization-token"), ("ecr-public", "get-login-password"),
    ("ecr-public", "get-authorization-token"), ("codeartifact", "get-authorization-token"),
    ("rds", "generate-db-auth-token"), ("eks", "get-token"), ("sts", "assume-role"),
    ("sts", "assume-role-with-saml"), ("sts", "assume-role-with-web-identity"),
    ("sts", "get-session-token"), ("sts", "get-federation-token"), ("kms", "decrypt"),
    ("configure", "export-credentials"), ("ec2", "get-password-data"), ("sso", "get-role-credentials"),
    ("redshift", "get-cluster-credentials"), ("redshift", "get-cluster-credentials-with-iam"),
    ("redshift-serverless", "get-credentials"),
}


def aws(args):
    pos = positionals(args, AWS_GLOBAL_VALUE)
    if len(pos) < 2:
        return None
    service, op = pos[0], pos[1]
    rest = pos[2:]
    if (service, op) in AWS_SECRET or (service == "ssm" and op.startswith("get-parameter") and has(args, "--with-decryption")) \
            or (service == "configure" and op == "get" and rest and "secret" in rest[0]):
        return f"aws {service} {op} reads a secret"
    if service == "s3":
        if op == "ls":
            return None
        paths = positionals(args, AWS_GLOBAL_VALUE | S3_VALUE)[2:4]
        if op in ("cp", "sync") and len(paths) == 2 and not paths[1].startswith("s3://"):
            return None
        return f"aws s3 {op} changes or shares S3 data"
    if service in ("configure", "sso") or (service, op) in {("eks", "update-kubeconfig"), ("logs", "start-live-tail"),
                                                            ("logs", "start-query"), ("logs", "stop-query")}:
        return None
    if AWS_READ.match(op):
        return None
    return f"aws {service} {op} changes cloud resources"


KUBECTL_VALUE = {"-n", "--namespace", "--context", "--cluster", "--kubeconfig", "--user", "-s", "--server",
                 "--as", "--as-group", "--request-timeout", "-l", "--selector", "-o", "--output", "-c",
                 "--container", "--field-selector", "--sort-by", "--template", "-f", "--filename"}
KUBECTL_READ = {"get", "describe", "logs", "log", "top", "explain", "api-resources", "api-versions", "version",
                "cluster-info", "diff", "wait", "events", "completion", "plugin", "kustomize", "options",
                "help", "auth", "config", "rollout"}


def kubectl(args):
    pos = positionals(args, KUBECTL_VALUE)
    if not pos:
        return None
    sub = pos[0]
    dry = flag_value(args, "--dry-run")
    if sub in ("apply", "create", "patch", "replace", "delete") and dry in ("client", "server"):
        return None
    if sub == "cp" and len(pos) == 3 and ":" in pos[1] and ":" not in pos[2]:
        return None
    if sub not in KUBECTL_READ:
        return f"kubectl {sub} changes or reaches into a cluster"
    nxt = pos[1] if len(pos) > 1 else ""
    if sub == "rollout" and nxt not in ("status", "history"):
        return f"kubectl rollout {nxt} changes a cluster"
    if sub == "auth" and nxt not in ("can-i", "whoami"):
        return f"kubectl auth {nxt} changes RBAC"
    if sub == "config" and has(args, "--raw"):
        return "kubectl config view --raw prints credentials"
    if sub == "get" and any(re.search(r"(^|,)secrets?(\.v1)?(/|,|$)", p) for p in pos[1:]):
        out = flag_value(args, "-o", "--output") or ("go-template" if has(args, "--template") else None)
        if out and out not in ("name", "wide"):
            return "kubectl get secret with an output format prints secret data"
    return None


HELM_READ = {"list", "ls", "status", "get", "history", "hist", "show", "inspect", "template", "search", "repo",
             "lint", "version", "dependency", "dep", "env", "help", "completion", "verify", "package", "pull",
             "fetch", "plugin", "diff", "create", "registry"}


def helm(args):
    pos = positionals(args, {"-n", "--namespace", "--kube-context", "--kubeconfig", "-f", "--values", "--version"})
    if pos and pos[0] not in HELM_READ and not has(args, "--dry-run"):
        return f"helm {pos[0]} changes a cluster or registry"
    return None


TF_READ = {"plan", "validate", "fmt", "init", "show", "output", "providers", "version", "graph", "console", "get",
           "login", "logout", "modules", "metadata", "help"}


def terraform(args, name="terraform"):
    pos = positionals(args)
    if not pos:
        return None
    sub, nxt = pos[0], (pos[1] if len(pos) > 1 else "")
    if name == "terragrunt" and sub in ("run-all", "run"):
        return terraform([a for a in args if a not in ("run-all", "run", "--")], name)
    if sub in TF_READ or (sub == "state" and nxt in ("list", "show", "pull")) \
            or (sub == "workspace" and nxt in ("list", "show", "select")):
        return None
    return f"{name} {' '.join(pos[:2])} changes infrastructure or state"


PULUMI_READ = {"preview", "pre", "whoami", "version", "about", "logs", "help", "plugin", "schema", "convert",
               "gen-completion", "login", "logout", "org", "install"}


def pulumi(args):
    if has(args, "--show-secrets"):
        return "pulumi --show-secrets prints secrets"
    pos = positionals(args, {"-C", "--cwd", "-s", "--stack"})
    if not pos:
        return None
    sub, nxt = pos[0], (pos[1] if len(pos) > 1 else "")
    if sub in PULUMI_READ or (sub == "stack" and nxt in ("", "ls", "select", "output", "history", "graph", "export")) \
            or (sub == "config" and nxt == ""):
        return None
    return f"pulumi {' '.join(pos[:2])} changes infrastructure, config or state"


def gh_api(args):
    if has(args, "--input"):
        return "gh api --input sends a request body"
    fields = [args[i + 1] for i, a in enumerate(args[:-1]) if a in ("-f", "-F", "--field", "--raw-field")]
    fields += [a.split("=", 1)[1] for a in args if a.startswith(("--field=", "--raw-field="))]
    fields += [a[2:] for a in args if re.match(r"^-[fF].", a)]
    pos = positionals(args, {"-X", "--method", "-f", "-F", "--field", "--raw-field", "-H", "--header", "-q",
                             "--jq", "-t", "--template", "--hostname", "--cache", "-p", "--preview"})
    if pos and pos[0] == "graphql":
        for f in fields:
            key, _, value = f.partition("=")
            if key == "query" and (value.startswith("@") or re.match(r"^\s*(#[^\n]*\n\s*)*mutation\b", value)):
                return "gh api graphql mutation changes GitHub state"
        return None
    method = (flag_value(args, "-X", "--method") or ("POST" if fields else "GET")).upper()
    if method not in ("GET", "HEAD"):
        return f"gh api {method} changes GitHub state"
    return None


GH_ASK_VERBS = {"delete", "archive", "unarchive", "rename", "transfer", "lock", "cancel", "set", "remove",
                "edit-protection", "sync"}


def gh(args):
    pos = positionals(args, {"-R", "--repo", "-b", "--body", "-t", "--title", "-B", "--base", "-H", "--head",
                             "-l", "--label", "-a", "--assignee", "-m", "--milestone", "-s", "--state", "-L",
                             "--limit", "-q", "--jq", "--json", "-F", "--body-file", "-w", "--workflow"})
    if not pos:
        return None
    group, sub = pos[0], (pos[1] if len(pos) > 1 else "")
    if group == "api":
        return gh_api(args[args.index("api") + 1:])
    if group == "auth" and sub == "token":
        return "gh auth token prints a credential"
    if group in ("secret", "variable") and sub not in ("list", "get"):
        return f"gh {group} {sub} changes repository settings"
    if sub in GH_ASK_VERBS:
        return f"gh {group} {sub} changes shared GitHub state"
    if group == "pr" and sub == "merge" and not has(args, "--auto", "--disable-auto"):
        return "gh pr merge merges immediately"
    if group == "pr" and sub == "review" and has(args, "--approve", "-a"):
        return "gh pr review --approve approves a pull request"
    if group == "pr" and sub == "close":
        return "gh pr close closes a pull request"
    if group == "repo" and sub in ("create", "edit"):
        return f"gh repo {sub} changes a repository"
    if group == "release" and sub in ("create", "upload", "edit"):
        return f"gh release {sub} publishes a release"
    if group == "workflow" and sub in ("run", "enable", "disable"):
        return f"gh workflow {sub} triggers or changes CI"
    return None


PROTECTED = re.compile(r"^(\+?)(?:[^:]*:)?(?:refs/heads/)?(main|master)$")


def git(args):
    pos = positionals(args, {"-C", "-c", "--git-dir", "--work-tree", "--namespace"})
    if not pos:
        return None
    sub = pos[0]
    if sub == "push":
        refspecs = pos[2:]
        if has(args, "--delete", "-d", "--mirror", "--prune") or any(r.startswith(":") for r in refspecs):
            return "git push deletes or mirrors remote refs"
        if any(r.startswith("+") for r in refspecs):
            return "git push +refspec force-pushes"
        if any(PROTECTED.match(r) for r in refspecs):
            return "git push to main/master changes the shared default branch"
    if sub == "credential" and len(pos) > 1 and pos[1] in ("fill", "get"):
        return "git credential prints a credential"
    if sub == "send-email":
        return "git send-email sends mail"
    return None


OP_READ = {"whoami", "signin", "signout", "account", "vault", "user", "group", "help", "completion", "update", "plugin"}


def op(args):
    pos = positionals(args, {"--account", "--vault", "--format", "--categories", "--tags"})
    if not pos or pos[0] in OP_READ or (pos[0] == "item" and pos[1:2] == ["list"]):
        return None
    return f"op {' '.join(pos[:2])} reads or changes a secret"


def gcloud(args):
    pos = [a for a in args if not a.startswith("-")]
    if any(p.startswith("print-") and p.endswith("token") for p in pos) or pos[:3] == ["secrets", "versions", "access"]:
        return "gcloud prints a token or secret"
    if not pos or pos[0] in ("config", "auth", "info", "version", "help", "components", "topic"):
        return None
    if pos[:2] == ["storage", "cp"] or pos[:2] == ["storage", "rsync"]:
        return None if not pos[-1].startswith("gs://") else "gcloud storage writes to Cloud Storage"
    if any(p in ("list", "describe", "get-iam-policy", "get-value", "read", "tail", "ls", "cat", "get-credentials")
           or p.startswith(("list-", "describe-")) for p in pos):
        return None
    return f"gcloud {' '.join(pos[:3])} changes cloud resources"


def gsutil(args):
    pos = positionals(args, {"-o", "-h", "-u"})
    if not pos or pos[0] in ("ls", "cat", "du", "stat", "hash", "version", "help"):
        return None
    if pos[0] in ("cp", "rsync") and not pos[-1].startswith("gs://"):
        return None
    return f"gsutil {pos[0]} changes Cloud Storage"


HTTP_WRITE_FLAGS = {"-d", "--data", "--data-raw", "--data-binary", "--data-urlencode", "--json", "-F", "--form",
                    "--form-string", "-T", "--upload-file", "--post-data", "--post-file", "--body-data", "--body-file"}


def http_client(args, name):
    urls = [a for a in args if re.match(r"^(https?://|[\w.-]+\.\w+(:\d+)?/|localhost|\[?::1|127\.)", a)]
    method = flag_value(args, "-X", "--request", "--method")
    if name in ("http", "https", "xh"):
        pos = positionals(args)
        if pos and pos[0].upper() in ("POST", "PUT", "PATCH", "DELETE"):
            method = pos[0]
        elif any(re.match(r"^[^=:]+(:=|=(?!=))", p) for p in pos[1:]) or has(args, "-f", "--form"):
            method = method or "POST"
    else:
        if name == "curl" and method is None:
            for i, a in enumerate(args):
                m = re.match(r"^-[A-Za-z]*X(.*)$", a)
                if m and not a.startswith("--"):
                    method = m.group(1) or (args[i + 1] if i + 1 < len(args) else None)
        bundled = name == "curl" and any(re.match(r"^-[A-Za-z]*[dFT]", a) and not a.startswith("--") for a in args)
        if bundled or any(a in HTTP_WRITE_FLAGS or a.split("=", 1)[0] in HTTP_WRITE_FLAGS for a in args):
            method = method or "POST"
    method = (method or "GET").upper()
    if method in ("GET", "HEAD", "OPTIONS"):
        return None
    if urls and all(is_local(host_of(u)) for u in urls):
        return None
    return f"{name} {method} writes to a remote service"


SQL_WRITE = re.compile(r"\b(insert|update|delete|drop|truncate|alter|(?<!show\s)create|grant|revoke|replace|merge"
                       r"|optimize|rename|attach|detach|kill|system(?!\.)|copy|vacuum|reindex|cluster)\b", re.I)
MONGO_WRITE = re.compile(r"\.(insert|update|delete|remove|replace|drop|create|rename|bulkWrite|findOneAnd)\w*\s*\(")
SQL_FLAGS = {"psql": ("-c", "--command"), "mysql": ("-e", "--execute"), "mariadb": ("-e", "--execute"),
             "clickhouse-client": ("-q", "--query"), "mongosh": ("--eval",), "mongo": ("--eval",),
             "cqlsh": ("-e", "--execute")}
REDIS_READ = {"get", "mget", "keys", "scan", "info", "ping", "ttl", "pttl", "type", "exists", "hget", "hgetall",
              "hkeys", "hlen", "hmget", "lrange", "llen", "smembers", "scard", "zrange", "zcard", "zscore", "strlen",
              "dbsize", "memory", "slowlog", "xrange", "xlen", "xinfo", "sscan", "hscan", "zscan",
              "--scan", "--bigkeys", "--memkeys", "--stat", "--latency"}


def db_client(args, name):
    if name == "redis-cli":
        host = flag_value(args, "-h", "-u") or ""
        if is_local(host_of(host)):
            return None
        pos = positionals(args, {"-h", "-p", "-a", "-u", "-n", "--user", "--pass"})
        if pos and pos[0].lower() in REDIS_READ:
            return None
        return "redis-cli changes a remote Redis"
    host = flag_value(args, "-h", "--host") or ""
    uri = next((m.group(0) for a in args for m in [re.search(r"[a-z]+(\+\w+)?://\S*", a)] if m), "")
    conninfo = next((m.group(1) for a in args for m in [re.search(r"\bhost=(\S+)", a)] if m), "")
    if is_local(host_of(host)) and is_local(host_of(uri)) and is_local(conninfo):
        return None
    sql = flag_value(args, *SQL_FLAGS[name])
    if sql is None or SQL_WRITE.search(sql) or (name in ("mongosh", "mongo") and MONGO_WRITE.search(sql)):
        return f"{name} may change a remote database"
    return None


def docker(args):
    pos = positionals(args, {"-H", "--host", "--context", "-t", "--tag", "-f", "--file", "--platform",
                             "--build-arg", "--target", "-o", "--output"})
    if "push" in pos[:2] or has(args, "--push") or any("type=registry" in a for a in args) \
            or pos[:3] == ["buildx", "imagetools", "create"]:
        return "pushes an image to a registry"
    return None


PUBLISH = {
    "npm": {"publish", "unpublish", "deprecate", "dist-tag", "owner", "access"},
    "pnpm": {"publish"}, "yarn": {"publish"}, "cargo": {"publish", "yank", "owner"},
    "twine": {"upload"}, "gem": {"push", "yank"}, "poetry": {"publish"}, "uv": {"publish"},
    "hatch": {"publish"}, "flit": {"publish"}, "mvn": {"deploy"},
    "crane": {"push", "delete", "tag", "copy", "cp", "append", "mutate", "rebase", "flatten"},
    "oras": {"push", "attach", "tag", "copy", "cp"}, "skopeo": {"copy", "delete", "sync"},
}


def publisher(args, name):
    pos = positionals(args, {"-x", "--exclude-task"})
    if name in ("gradle", "gradlew"):
        hit = [p for p in pos if p.split(":")[-1].startswith("publish") and "MavenLocal" not in p]
        return f"{name} {hit[0]} publishes artifacts" if hit else None
    if pos and (pos[0] in PUBLISH[name] or (name == "yarn" and pos[:2] == ["npm", "publish"])):
        return f"{name} {pos[0]} publishes or changes a registry"
    if name == "oras" and pos[:1] in (["manifest"], ["blob"]) and pos[1:2] in (["push"], ["delete"]):
        return f"oras {pos[0]} {pos[1]} changes a registry"
    return None


def remote_copy(args, name):
    if name == "sftp":
        return "sftp changes remote files"
    pos = positionals(args, {"-e", "-i", "-P", "-p", "-o", "-F", "--rsh"})
    if pos and re.match(r"^[^/.][^/]*:", pos[-1]):
        return f"{name} writes to a remote host"
    return None


def ssh(args):
    pos = positionals(args, {"-i", "-p", "-l", "-o", "-F", "-J", "-L", "-R", "-D", "-W", "-b", "-c", "-E", "-m",
                             "-O", "-Q", "-S", "-w", "-B", "-e", "-I"})
    if has(args, "-V", "-G") or (pos and pos[0] in ("git@github.com", "github.com") and len(pos) == 1):
        return None
    return "ssh runs commands on a remote host"


DEPLOYER_READ = {"get", "version", "auth", "logout", "readiness", "generate", "self-update", "help", "completion"}


def deployer(args):
    pos = positionals(args)
    if not pos or pos[0] in DEPLOYER_READ:
        return None
    if pos[0] == "run":
        preview = flag_value(args, "--preview")
        return "deployer run --preview=false applies changes" if preview in ("false", "0") else None
    return f"deployer {pos[0]} changes environments or secrets"


def wrangler(args):
    pos = positionals(args)
    if not pos or pos[0] in ("whoami", "dev", "tail", "types", "login", "logout", "docs", "help", "init") \
            or pos[-1] in ("list", "get") or has(args, "--version", "-v"):
        return None
    return f"wrangler {' '.join(pos[:3])} changes Cloudflare"


def flarectl(args):
    pos = positionals(args)
    if any(p in ("create", "update", "delete", "create-or-update", "set", "purge") for p in pos):
        return f"flarectl {' '.join(pos[:2])} changes Cloudflare"
    return None


def first_word(name, allowed, what):
    def check(args):
        pos = positionals(args, {"-n", "--namespace", "-f", "--file", "-e", "--environment", "-g", "--resource-group",
                                 "--project", "--profile", "--region", "-p", "--context"})
        if not pos or pos[0] in allowed or any(w in allowed for w in pos[1:2]) or has(args, "--dry-run", "--check",
                                                                                      "--syntax-check", "--list-tasks",
                                                                                      "--list-hosts", "--version", "-h", "--help"):
            return None
        return f"{name} {' '.join(pos[:2])} {what}"
    return check


def az(args):
    pos = [a for a in args if not a.startswith("-")]
    if not pos or pos[0] in ("login", "logout", "account", "config", "version", "help", "upgrade", "extension") \
            and "get-access-token" not in pos:
        return None
    if any(p in ("list", "show", "get", "query", "download") or p.startswith(("list-", "show-")) for p in pos) \
            and not any(p in ("get-access-token", "keys", "secret") for p in pos):
        return None
    return f"az {' '.join(pos[:3])} changes Azure resources or reads secrets"


def rclone(args):
    pos = positionals(args, {"--config", "--include", "--exclude", "--filter", "--transfers", "--checkers"})
    if not pos or pos[0] in ("ls", "lsd", "lsl", "lsf", "lsjson", "cat", "size", "check", "about", "tree", "version",
                             "listremotes", "help", "md5sum", "sha1sum", "hashsum", "ncdu"):
        return None
    if pos[0] in ("copy", "copyto", "sync", "move", "moveto", "bisync") and len(pos) >= 3 \
            and not re.match(r"^[^/.][^/]*:", pos[2]):
        return "rclone moves files out of a remote" if pos[0].startswith("move") else None
    return f"rclone {pos[0]} changes remote storage"


def env_dump(args, name):
    if name in ("env", "printenv") and not [a for a in args if not a.startswith("-")]:
        return f"{name} dumps the environment, which may hold tokens"
    if name in ("export", "declare", "typeset", "set") and (not args or args in (["-p"], ["-x"], ["-px"])):
        return f"{name} dumps the environment, which may hold tokens"
    if name == "printenv" and any(SECRET_VAR.search("$" + a) for a in args):
        return "printenv prints a secret"
    return None


def bd(args):
    if positionals(args)[:2] == ["dolt", "push"]:
        return "bd dolt push publishes the beads database"
    return None


CLASSIFIERS = {
    "aws": aws, "kubectl": kubectl, "oc": kubectl, "helm": helm, "pulumi": pulumi, "gh": gh, "git": git,
    "op": op, "gcloud": gcloud, "gsutil": gsutil, "ssh": ssh, "deployer": deployer, "wrangler": wrangler,
    "flarectl": flarectl, "bd": bd, "docker": docker, "podman": docker, "nerdctl": docker,
    "terraform": terraform, "tofu": lambda a: terraform(a, "tofu"), "terragrunt": lambda a: terraform(a, "terragrunt"),
    "helmfile": first_word("helmfile", {"diff", "template", "lint", "list", "status", "build", "deps", "repos",
                                        "write-values", "show-dag", "fetch", "version", "help"}, "changes a cluster"),
    "argocd": first_word("argocd", {"get", "list", "diff", "history", "logs", "manifests", "resources", "version",
                                    "login", "logout", "context", "account", "help", "wait"}, "changes deployments"),
    "eksctl": first_word("eksctl", {"get", "utils", "info", "version", "help", "completion"}, "changes EKS"),
    "cdk": first_word("cdk", {"synth", "synthesize", "diff", "ls", "list", "doctor", "docs", "context", "notices",
                              "acknowledge", "metadata", "version", "init"}, "changes cloud resources"),
    "ansible-playbook": lambda a: None if has(a, "--check", "-C", "--syntax-check", "--list-tasks", "--list-hosts",
                                              "--list-tags") else "ansible-playbook changes remote hosts",
    "bq": first_word("bq", {"ls", "show", "head", "help", "version", "wait", "init"}, "changes BigQuery"),
    "sops": lambda a: "sops decrypts secrets" if has(a, "-d", "--decrypt", "exec-env", "exec-file") or
                      positionals(a)[:1] in (["decrypt"], ["exec-env"], ["exec-file"]) else None,
    "doppler": lambda a: "doppler reads or injects secrets" if positionals(a)[:1] in (["secrets"], ["run"]) else None,
    "az": az, "rclone": rclone,
    "vault": lambda a: None if positionals(a)[:1] in ([], ["status"], ["version"], ["path-help"]) else "vault reads or writes secrets",
    "mail": lambda a: "sends mail", "sendmail": lambda a: "sends mail", "mutt": lambda a: "sends mail",
}
for _n in ("curl", "wget", "http", "https", "xh"):
    CLASSIFIERS[_n] = lambda a, n=_n: http_client(a, n)
for _n in ("pg_restore",):
    CLASSIFIERS[_n] = lambda a: None if is_local(flag_value(a, "-h", "--host") or "") else "pg_restore writes to a remote database"
for _n in ("psql", "mysql", "mariadb", "clickhouse-client", "redis-cli", "mongosh", "mongo", "cqlsh"):
    CLASSIFIERS[_n] = lambda a, n=_n: db_client(a, n)
for _n in list(PUBLISH) + ["gradle", "gradlew"]:
    CLASSIFIERS[_n] = lambda a, n=_n: publisher(a, n)
for _n in ("scp", "rsync", "sftp"):
    CLASSIFIERS[_n] = lambda a, n=_n: remote_copy(a, n)
for _n in ("env", "printenv", "export", "declare", "typeset", "set"):
    CLASSIFIERS[_n] = lambda a, n=_n: env_dump(a, n)


# --- wrappers -----------------------------------------------------------------

def skip_opts(args, value_opts=()):
    i = 0
    while i < len(args) and args[i].startswith("-") and args[i] != "--":
        i += 2 if args[i] in value_opts else 1
    if i < len(args) and args[i] == "--":
        i += 1
    return args[i:]


def after_dashdash(args):
    return args[args.index("--") + 1:] if "--" in args else None


def unwrap(tok):
    """Return (inner token list, extra command strings) or None when tok is not a wrapper."""
    name, args = os.path.basename(tok[0]), tok[1:]
    if name in ("bash", "sh", "zsh", "dash", "ksh"):
        for i, a in enumerate(args):
            if re.match(r"^-[a-zA-Z]*c[a-zA-Z]*$", a) and i + 1 < len(args):
                return [], [args[i + 1]]
        return None
    if name == "eval":
        return [], [" ".join(args)]
    if name == "sudo":
        return skip_opts(args, {"-u", "-g", "-U", "-C", "-D", "-h", "-p", "-r", "-t", "-T"}), []
    if name == "env":
        s = flag_value(args, "-S", "--split-string")
        if s is not None:
            return [], [s]
        rest = skip_opts(args, {"-u", "--unset", "-C", "--chdir"})
        while rest and ASSIGNMENT.match(rest[0]):
            rest = rest[1:]
        return (rest, []) if rest else None
    if name == "timeout":
        rest = skip_opts(args, {"-s", "--signal", "-k", "--kill-after"})
        return rest[1:], []
    if name in ("nice", "ionice", "stdbuf", "nohup", "time", "command", "exec", "builtin", "chronic", "unbuffer"):
        rest = skip_opts(args, {"-n", "-c", "-p", "-o", "-e", "-i"})
        return (rest, []) if rest else None
    if name == "xargs":
        return skip_opts(args, {"-I", "-i", "-n", "-P", "-d", "-L", "-l", "-s", "-a", "-E", "-e", "--delimiter",
                                "--max-args", "--max-procs", "--arg-file", "--replace"}), []
    if name == "watch":
        rest = skip_opts(args, {"-n", "--interval", "-d"})
        return ([], [rest[0]]) if len(rest) == 1 else (rest, [])
    if name in ("npx", "bunx", "pnpx"):
        rest = skip_opts(args, {"-p", "--package"})
        return (rest, []) if rest else None
    if name in ("mise", "aws-vault", "direnv", "doppler", "devbox", "uv", "poetry", "pipenv"):
        inner = after_dashdash(args)
        if inner is not None:
            return inner, []
        if args[:1] == ["run"] and name != "mise":
            rest = skip_opts(args[1:], {"--with", "--python", "-p", "--project", "--directory", "--package"})
            return (rest, []) if rest else None
        return None
    if name == "find":
        cmds, i = [], 0
        while i < len(args):
            if args[i] in ("-exec", "-execdir", "-ok", "-okdir"):
                j = i + 1
                while j < len(args) and args[j] not in (";", "+"):
                    j += 1
                cmds.append(shlex.join(args[i + 1:j]))
                i = j
            i += 1
        return ([], cmds) if cmds else None
    return None


DB_HOST_ENV = {"PGHOST": "psql", "MYSQL_HOST": "mysql", "CLICKHOUSE_HOST": "clickhouse-client"}


def classify_tokens(raw, depth):
    tok = strip_noise(raw)
    env_host = [a.split("=", 1)[1] for a in raw[:raw.index(tok[0])] if a.split("=", 1)[0] in DB_HOST_ENV] if tok else []
    if env_host and os.path.basename(tok[0]) in DB_HOST_ENV.values():
        tok = [tok[0], "-h", env_host[0]] + tok[1:]
    if not tok:
        return None
    if os.path.basename(tok[0]) in ("op", "doppler") and tok[1:2] == ["run"]:
        return f"{os.path.basename(tok[0])} run injects secrets"
    wrapped = unwrap(tok)
    if wrapped is not None:
        inner, strings = wrapped
        for s in strings:
            reason = classify(s, depth + 1)
            if reason:
                return reason
        return classify_tokens(inner, depth + 1) if inner else None
    name = os.path.basename(tok[0])
    if name not in PATH_ONLY:
        for t in raw[raw.index(tok[0]) + 1:]:
            if SECRET_PATH.search(os.path.expanduser(t)):
                return f"{name} reads a credential file ({t})"
    if name in ("echo", "printf", "print") and any(SECRET_VAR.search(t) for t in tok[1:]):
        return f"{name} prints a secret variable"
    fn = CLASSIFIERS.get(name)
    return fn(tok[1:]) if fn else None


def classify(command, depth=0):
    """Return a reason to ask, or None to allow."""
    if depth > MAX_DEPTH:
        return "command nests too deeply to classify"
    command, scripts = strip_heredocs(command)
    segments, nested = split(command)
    for n in scripts + nested:
        reason = classify(n, depth + 1)
        if reason:
            return reason
    for seg in segments:
        reason = classify_tokens(tokens(seg), depth)
        if reason:
            return reason
    return None
