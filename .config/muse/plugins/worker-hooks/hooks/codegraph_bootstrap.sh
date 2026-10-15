#!/bin/sh
# SessionStart: the CodeGraph worktree bootstrap Claude and Codex run. Its
# input and output already match Muse's hook protocol. /usr/bin/node, as in
# their hook configs: codegraph's native SQLite module is built for it, not
# for whichever node is first on PATH.
script="$HOME/.codex/hooks/codegraph-worktree-bootstrap.js"
node=/usr/bin/node
[ -x "$node" ] || node=$(command -v node) || exit 0
[ -f "$script" ] || exit 0
exec "$node" "$script"
