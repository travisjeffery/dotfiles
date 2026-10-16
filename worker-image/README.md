# Worker image

A container image for running [quartermaster](https://github.com/travisjeffery/herdr-quartermaster)
workers on any machine that has a container runtime: a headless Herdr server
plus every tool a worker agent uses, so a host needs nothing installed but
Docker and SSH. The design is in quartermaster's
`docs/design/worker-hosts.md`.

## What's in it

| | Pinned in |
|---|---|
| Herdr (headless server, `herdr server` is the default command) | `Dockerfile` (`HERDR_VERSION` + SHA-256 per arch) |
| quartermaster (built from a pinned commit) | `Dockerfile` (`QUARTERMASTER_REF`) |
| mise | `Dockerfile` (`MISE_VERSION` + SHA-256 per arch) |
| Go, Node, uv, gh, golangci-lint, gopls, goimports, Helm, Claude Code, Codex, codegraph | `mise.toml` |
| Global `CLAUDE.md` / `AGENTS.md`, Claude commands and hooks, Codex hooks, Muse plugins, `.gitconfig` | this repository, linked from `/opt/dotfiles` on every start |
| Worker Claude settings, Codex config and hooks | `config/` (generic: this repository is public) |

Not yet in it: `bd` (workers reach the desktop's Beads database through a
forced-command SSH proxy, added in a later phase as `/usr/local/bin/bd`), and
Muse (its launcher can't be pinned to a version yet).

A repository's own `go.mod` still picks its Go: mise installs other versions
on demand into the cache volume.

## What's not in it

No credentials and no state, ever. CI scans the built image with gitleaks and
fails on a finding.

## Running it

```sh
docker run -d --name qm-worker --restart unless-stopped \
  --memory 12g \
  -v qm-src:/home/tj/src \
  -v qm-worktrees:/home/tj/worktrees \
  -v qm-cache:/home/tj/.cache \
  -v qm-claude:/home/tj/.claude \
  -v qm-codex:/home/tj/.codex \
  -v qm-herdr:/home/tj/.config/herdr \
  -v "$HOME/.config/qm-worker/secrets:/run/qm-secrets:ro" \
  ghcr.io/travisjeffery/qm-worker:<version>
```

The entrypoint, on every start:

1. gives fresh volume mount points to the worker user;
2. links the dotfiles from `/opt/dotfiles` into `$HOME` and copies the
   worker agent settings (the agents write to those);
3. copies credentials from `/run/qm-secrets` into `~/.config/qm-worker`
   (0600, owned by the worker), sets git to use `gh` for GitHub HTTPS and to
   sign with this host's key (`signing-key.pub`), or not to sign without one;
4. runs the command as the worker user (uid 1000). Under rootless Docker,
   container root is the host user, so files on volumes stay the host user's.

Credentials it understands, all optional:

| File in `/run/qm-secrets` | Used for |
|---|---|
| `gh-token` | `GH_TOKEN` for `gh` and git over HTTPS |
| `signing-key`, `signing-key.pub` | this host's SSH commit-signing key |
| `beads-key` | the Beads proxy (later phase) |
| `known_hosts` | hosts the worker may SSH to |

Agent logins (Claude, Codex) are done once per host and persist on the
`qm-claude` / `qm-codex` volumes:
`docker exec -it qm-worker claude /login`, `docker exec -it qm-worker codex login --device-auth`.

## Building

CI (`.github/workflows/worker-image.yml`) builds on changes to this directory
or to the dotfiles it bakes in, checks the manifest is pinned, runs
`smoke.sh` inside the image, scans for secrets, and on `master` pushes
`ghcr.io/travisjeffery/qm-worker:<yyyy.mm.dd>-<sha>` and `latest`. Hosts pin
a dated tag, never `latest`.

Locally:

```sh
docker build -f worker-image/Dockerfile -t qm-worker:dev .
docker run --rm qm-worker:dev smoke.sh
```

To bump a tool, change its version in `mise.toml` (or the `ARG` in the
`Dockerfile`, with its checksums) and open a PR; `check-manifest.sh` refuses
`latest` and partial versions.
