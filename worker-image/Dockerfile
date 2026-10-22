# syntax=docker/dockerfile:1.7
#
# Quartermaster worker image: a headless Herdr server plus everything a
# worker agent uses. No credentials and no state: those arrive at runtime as
# mounts (see README.md). Build from the repository root:
#
#   docker build -f worker-image/Dockerfile -t qm-worker .

ARG DEBIAN=debian:trixie-slim@sha256:a29215f6a35e51e22adffa17f89e9d2ef06214e64a2bad10d765c46aea49f11f
ARG GOLANG=golang:1.26.7-trixie@sha256:978084e7adea0904b6d76cbc9afcb9baeed6ee170eecdc8866566aaa9f22ad0c

# quartermaster is built from a pinned commit, so the image doesn't wait on a
# release and matches what the desktop runs from source.
FROM ${GOLANG} AS quartermaster
ARG QUARTERMASTER_REF=e2620434718903f467ea7239325d2ef2db38d176
RUN git clone --filter=blob:none https://github.com/travisjeffery/herdr-quartermaster /src \
 && git -C /src checkout --detach "${QUARTERMASTER_REF}" \
 && cd /src && CGO_ENABLED=0 go build -trimpath -ldflags "-s -w -X main.version=$(git describe --tags --always)" -o /out/quartermaster . \
 && ln -s quartermaster /out/qm

FROM ${DEBIAN}
ARG TARGETARCH
ARG HERDR_VERSION=0.9.1
ARG HERDR_SHA256_AMD64=2a02fed16beb651ef006e1d43f048f652ca4dc58ad053cd2d44450563d5c54b7
ARG HERDR_SHA256_ARM64=f4ccf4de745f2cb9a39a983e9ba3703dad50ec2a58dea83026ceab721bbd8d9e
ARG MISE_VERSION=2026.8.11
ARG MISE_SHA256_AMD64=3d825b8341925abb398fba6fd8dcd7d6e92ade6820d892da62992e0765638e66
ARG MISE_SHA256_ARM64=6b7471271a990cbd6a795b24f9df83338aa220c227bf75fd083442e5a728f5f7
# The worker user matches the host user's uid so files on bind mounts keep
# their owner under rootful Docker. Under rootless Docker the entrypoint runs
# as container root (the host user) and drops to this uid.
ARG USER=tj
ARG UID=1000

RUN apt-get update \
 && apt-get install -y --no-install-recommends \
      ca-certificates curl git git-lfs openssh-client jq ripgrep less \
      python3 build-essential procps tini util-linux xz-utils unzip zsh \
 && rm -rf /var/lib/apt/lists/*

RUN set -eu; \
    case "${TARGETARCH:-amd64}" in \
      amd64) h=x86_64;  hs="${HERDR_SHA256_AMD64}"; m=x64;   ms="${MISE_SHA256_AMD64}" ;; \
      arm64) h=aarch64; hs="${HERDR_SHA256_ARM64}"; m=arm64; ms="${MISE_SHA256_ARM64}" ;; \
      *) echo "unsupported arch ${TARGETARCH}" >&2; exit 1 ;; \
    esac; \
    curl -fsSL "https://github.com/herdrdev/herdr/releases/download/v${HERDR_VERSION}/herdr-linux-${h}" -o /usr/local/bin/herdr; \
    echo "${hs}  /usr/local/bin/herdr" | sha256sum -c -; \
    curl -fsSL "https://github.com/jdx/mise/releases/download/v${MISE_VERSION}/mise-v${MISE_VERSION}-linux-${m}" -o /usr/local/bin/mise; \
    echo "${ms}  /usr/local/bin/mise" | sha256sum -c -; \
    chmod 0755 /usr/local/bin/herdr /usr/local/bin/mise

COPY --from=quartermaster /out/ /usr/local/bin/

# Toolchains go to the system-wide shared directory, which every user's mise
# searches; on-demand installs go to MISE_DATA_DIR on the cache volume.
COPY worker-image/mise.toml /etc/mise/config.toml
ENV MISE_SYSTEM_DATA_DIR=/usr/local/share/mise \
    MISE_YES=1
# Shims live with the system installs so they are on PATH for every user.
RUN mise trust /etc/mise/config.toml && mise install --system \
 && MISE_DATA_DIR=/usr/local/share/mise mise reshim \
 && ls /usr/local/share/mise/shims/claude /usr/local/share/mise/shims/go

RUN useradd --create-home --uid "${UID}" --shell /bin/zsh "${USER}"
# Every directory that holds a volume or bind mount, and its parents, exists
# owned by the worker. Otherwise the runtime creates missing parents as root,
# and a container started straight as the worker (Podman --userns=keep-id)
# can't write under them.
RUN cd "/home/${USER}" && install -d -o "${USER}" -g "${USER}" \
      src worktrees .cache .claude .codex .config .config/herdr .config/muse \
      .config/qm-worker .local .local/share .ssh

# The dotfiles a worker needs, kept read-only under /opt/dotfiles and linked
# into $HOME by the entrypoint on every start, so a stale volume can't pin
# old config.
COPY .gitconfig /opt/dotfiles/.gitconfig
COPY .claude/CLAUDE.md /opt/dotfiles/.claude/CLAUDE.md
COPY .claude/commands /opt/dotfiles/.claude/commands
COPY .claude/hooks /opt/dotfiles/.claude/hooks
COPY .codex/AGENTS.md .codex/herdr-agent-state.sh /opt/dotfiles/.codex/
COPY .config/muse/plugins /opt/dotfiles/.config/muse/plugins
COPY worker-image/config /opt/dotfiles/worker
COPY worker-image/entrypoint.sh worker-image/smoke.sh /usr/local/bin/

ENV QM_USER=${USER} \
    HERDR_VERSION=${HERDR_VERSION} \
    HOME=/home/${USER} \
    PATH=/usr/local/share/mise/shims:/usr/local/bin:/usr/bin:/bin \
    MISE_DATA_DIR=/home/${USER}/.cache/mise \
    GOPATH=/home/${USER}/.cache/go \
    npm_config_cache=/home/${USER}/.cache/npm \
    LANG=C.UTF-8

LABEL org.opencontainers.image.source=https://github.com/travisjeffery/dotfiles \
      org.opencontainers.image.description="Quartermaster worker: headless Herdr server and agent toolchain"

ENTRYPOINT ["/usr/bin/tini", "--", "/usr/local/bin/entrypoint.sh"]
# Herdr reports itself ready for saved SSH machines ("detached server daemon")
# only when it leads its own session; -w keeps setsid in the foreground.
CMD ["setsid", "-w", "herdr", "server"]
