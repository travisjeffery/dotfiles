# CodeGraph worktree bootstrap

The user-level Codex `SessionStart` hook in `~/.codex/hooks.json` runs
`codegraph-worktree-bootstrap.js` for startup, resume, and clear events.

For a Git worktree, the bootstrapper:

1. Identifies the repository through its shared Git common directory.
2. Reuses a compatible immutable seed for the installed CodeGraph version.
3. Installs the seed with a filesystem copy-on-write reflink through Node's
   `COPYFILE_FICLONE_FORCE`, falling back to a regular copy when reflinks are
   unavailable.
4. Reconciles the seed commit to the worktree commit using a NUL-safe Git
   name-status diff and CodeGraph's incremental file API.
5. Synchronizes tracked and untracked working-tree changes.
6. Verifies indexed file size, modification time, and content hash to repair
   transient edits that were later reverted and therefore no longer appear in
   `git status`.
7. Records provenance only after successful reconciliation.
8. Publishes a consistent SQLite backup as a future seed when the worktree is
   clean. At most three seeds are retained per repository and CodeGraph
   version. Worktrees install that seed through copy-on-write reflinks.

Normal session startup never performs a full index. When there is no compatible
seed, provenance is missing or incompatible, or a submodule commit changes, the
hook immediately fails open so a CodeGraph problem cannot delay the task. Full
indexing is an explicit maintenance operation protected by a repository-wide
lock. Codex also enforces a 30-second ceiling on the normal startup hook.

Local state and logs live under:

```text
~/.codex/codegraph-worktree-bootstrap/
```

Run the integration tests with:

```bash
/usr/bin/node --test ~/.codex/hooks/codegraph-worktree-bootstrap.test.js
```

Run the bootstrapper manually by passing a `SessionStart` payload:

```bash
printf '%s\n' \
  '{"hook_event_name":"SessionStart","cwd":"'"$PWD"'","source":"startup"}' \
  | CODEGRAPH_BOOTSTRAP_ALLOW_FULL_INDEX=1 \
    /usr/bin/node ~/.codex/hooks/codegraph-worktree-bootstrap.js
```

The explicit `/usr/bin/node` selects the installed Node 22 LTS runtime required
by CodeGraph 0.8.0; the interactive shell's newer Node version is left alone.
