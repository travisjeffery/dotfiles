# Global Codex Guidance

Global working agreements for Codex CLI.

## Accuracy, recency, and sourcing (REQUIRED)

When a request depends on recency (e.g., "latest", "current", "today", "as of now"):

1. **Establish the current date/time** and state it explicitly in ISO format.
   - Preferred: `date -Is` (timestamp).

2. **Prefer official / primary sources** when researching:
   - Upstream vendor docs for any dependency (language runtime, framework, cloud provider, etc.)

3. **Prefer the most recent authoritative information**:
   - Use the newest versioned docs, release notes, or changelogs.
   - Cross-check at least two reputable sources when details are safety/compatibility sensitive.

### Web search policy

- Enable and use web search only when it materially improves correctness (e.g., up-to-date APIs, recent advisories, release notes).
- Prefer official docs and primary sources.
- Record source dates (publish/release dates) when relevant.

## Default autonomy and safety

- Default to read-only exploration and analysis.
- When edits are needed, prefer **workspace-scoped** write access and keep changes inside the repo.
- When interacting with remote APIs, you must use READ-only calls, unless explicitily instructed otherwise by the user. If the user requests an API WRITE-based command, perform it as a dry-run first. You must never make destructive calls to remote APIs or production data sources.

### Editing files

- Make the smallest safe change that solves the issue.
- Preserve existing style and conventions.
- Prefer patch-style edits (small, reviewable diffs) over full-file rewrites.
- After making changes, run the project’s standard checks when feasible (format/lint, unit tests, build/typecheck).

### Reading project documents (PDFs, uploads, long text, CSVs, etc)

- Read the full document first.
- Draft the output.
- **Before finalizing**, re-read the original source to verify:
  - factual accuracy,
  - no invented details,
  - wording/style is preserved unless the user explicitly asked to rewrite.
- If paraphrasing is required, label it explicitly as a paraphrase.

### Container-first policy (REQUIRED)

- Codex must **never** install system packages on the host unless explicitly instructed.
- Prefer container images to supply all tooling used by the project.
- For code projects and dependencies: **use containers by default**.
- If the repo has an existing container workflow (Dockerfile/compose/Makefile targets), follow it.
- If the repo has no container workflow, create a minimal one.
- Keep repo-specific container details in the repo’s `AGENTS.md`.

### Secrets and sensitive data

- Never print secrets (tokens, private keys, credentials) to terminal output.
- Do not request users paste secrets.
- Avoid commands that might expose secrets (e.g., dumping env vars broadly, `cat ~/.ssh/*`).
- Prefer existing authenticated CLIs; redact sensitive strings in any displayed output.

## Baseline workflow

- Start every task by determining:
  1. Goal + acceptance criteria.
  2. Constraints (time, safety, scope).
  3. What must be inspected (files, commands, tests, docs).
  4. Whether the request depends on **recency** (if yes, apply the "Accuracy, recency, and sourcing" rules).
  5. If requirements are ambiguous, ask targeted clarifying questions before making irreversible changes.

### Beans and OpenMemory policy

- Beans is the canonical source of truth for active workspace continuity. OpenMemory is supplemental memory only.
- If `.beans/` and `.beans.yml` exist, run `beans prime` before doing anything else in this repo and heed its output.
- If Beans has not been initialized yet, prefer adopting it with `beans init` when the task includes repo-maintenance or workflow updates; otherwise note that Beans is not configured and continue with repo files plus OpenMemory.
- Read the relevant active and archived beans before starting substantive work. Use OpenMemory as additional context when available.
- If OpenMemory conflicts with Beans or repo files, Beans and repo files win.
- Persist critical task state in Beans rather than `.agent/STATE.md`, including plans, decisions, progress, discoveries, and outcomes.
- If Beans is unavailable, continue with repo files plus OpenMemory where available, and note the limitation explicitly.
- Do not commit Beans files.

### Beans Workflow

Update Beans whenever there is a meaningful delta in:

  - plans: what should happen next and what acceptance criteria or constraints changed.
  - decisions: choices made, tradeoffs accepted, and what was ruled out.
  - progress: material implementation status, blockers, and course corrections.
  - discoveries: notable findings, evidence, incompatibilities, or unexpected behavior that changed the approach.
  - outcomes: what shipped, what was verified, what remains, and any follow-up work.

When recording Beans context:

  - Prefer the `beans` CLI for creating and updating records. Preserve the schema it generates in `.beans/` and `.beans.yml`.
  - Keep updates factual, concise, and high-signal. No transcripts or raw logs.
  - Include ISO timestamps when recording dated facts.
  - Carry forward provenance in notes where useful: `[USER]`, `[CODE]`, `[TOOL]`, `[ASSUMPTION]`.
  - If something is unknown, mark it `UNCONFIRMED` instead of guessing.
  - Treat archived beans as project memory; consult them before repeating past work or revisiting old decisions.

### Anti-drift / anti-bloat rules

- Facts only, no transcripts, no raw logs.
- Every entry must include:
  - a date in ISO timestamp (e.g., `2026-01-13T09:42Z`)
  - a provenance tag: `[USER]`, `[CODE]`, `[TOOL]`, `[ASSUMPTION]`
  - If unknown, write `UNCONFIRMED` (never guess). If something changes, supersede it explicitly (don't silently rewrite history).
- Keep the file bounded, short and high-signal (anti-bloat). 
- If sections begin to become bloated, compress older items into milestone (`[MILESTONE]`) bullets.

### Pull Requests

- Don't comment that Codex created the pull request
- Don't comment that Codex made the commits
- Follow the repository's pull request template if it exists
- Keep the PR's title and description up-to-date with changes.
- When opening pull requests, look for related Linears, include link in Linear to PR, and link in PR to Linear.

## Definition of done

A task is done when:

- the requested change is implemented or the question is answered,
  - verification is provided:
  - build attempted (when source code changed),
  - linting run (when source code changed),
  - errors/warnings addressed (or explicitly listed and agreed as out-of-scope),
  - plus tests/typecheck as applicable,
- documentation is updated exhaustively for impacted areas,
- impact is explained (what changed, where, why),
- follow-ups are listed if anything was intentionally left out.
- Beans are updated if the change materially affects goals, decisions, progress, or follow-up scope.

## Context7 MCP (library docs)

Use Context7 to fetch accurate, version-matched documentation during coding tasks.

- Add `use context7` when you need library/API docs.
- If known, pin the library with slash syntax (e.g., `use library /supabase/supabase`).
- Mention the target version.
- Fetch minimal targeted docs; summarize (no large dumps).
