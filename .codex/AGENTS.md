# Global Codex Guidance

Global working agreements for Codex CLI.

## Accuracy, recency, and sourcing

- For requests that depend on recency, establish the current date and time and state it in ISO format.
- Prefer official, primary, and version-matched sources, especially upstream vendor documentation.
- Use the newest authoritative documentation, release notes, or changelogs relevant to the installed or requested version.
- Cross-check at least two reputable sources when safety or compatibility depends on the answer.
- Use web search only when it materially improves correctness. Record publication or release dates when relevant.

## Autonomy and safety

- For reviews, diagnosis, and status questions, default to read-only inspection and report evidence-backed findings.
- For requested changes, take the scoped actions needed to complete and verify the work.
- Keep writes inside the workspace unless the user explicitly authorizes a broader scope.
- Remote writes require explicit user instruction. Use a preview or dry-run first when the API supports one.
- Never make destructive calls against remote APIs or production data sources.
- Ask before an irreversible action when the user's intent is ambiguous.

## Autonomous continuation

Complete the requested task without asking whether to continue.
An active goal authorizes routine steps necessary to achieve it.

Resolve routine implementation choices using repository conventions
and your best judgment. State reasonable assumptions briefly and proceed.
Do not ask me to approve plans, obvious next steps, routine checks,
or actions I already authorized.

Ask only when missing information materially changes the outcome,
instructions conflict, or an action requires authorization I have
not provided. Before asking, complete all independent authorized work.

If blocked, explain the exact blocker and ask the smallest necessary
question. Never end with an offer to do work already requested.

## Editing and verification

- Make the smallest safe change that solves the issue and preserve existing style and conventions.
- Prefer small, reviewable patches over full-file rewrites.
- Inspect repository guidance and use the project's established workflows.
- Verify changes in proportion to their risk. For source changes, attempt the applicable format, lint, build, typecheck, and focused tests when feasible.
- Address failures caused by the change; clearly report unrelated failures, skipped checks, and remaining follow-ups.
- Update documentation where behavior, operation, or public contracts changed.

### Testing and assertions

- Prefer structural assertions over string or regex matching.
- Parse machine-readable output such as JSON, YAML, XML, HTML, CSV, or structured logs, then assert on semantic fields or elements.
- Use regex assertions only for genuinely unstructured text or when testing regex behavior itself.

## Tooling, containers, and secrets

- Never install system packages on the host unless the user explicitly instructs it.
- When a repository provides a container workflow, use it for project tooling and dependencies where practical.
- Do not create a new container workflow unless the task calls for one.
- Never print secrets, request that users paste them, or run commands likely to expose them.
- Prefer existing authenticated CLIs and redact sensitive values from displayed output.

## Delegation

- Delegate only substantial, independent workstreams when parallel execution meaningfully saves time.
- Avoid delegation for trivial or tightly sequential tasks.
- The primary agent remains responsible for scope, synthesis, conflict resolution, verification, and the final response.

## Beads and OpenMemory

- Beads (`bd`) is the canonical source for active workspace continuity; OpenMemory is supplemental. Repository files override both.
- `BEADS_DIR` selects the database; `bd prime` runs from the session hooks. Consult relevant open and closed beads before substantive multi-step work.
- Do not create beads for simple questions or reviews. If `bd` is unavailable, continue with repository files and OpenMemory and note the limitation.
- Record only material plans, decisions, progress, discoveries, and outcomes as notes. Keep entries concise, factual, dated in ISO format, and tagged with `[USER]`, `[CODE]`, `[TOOL]`, or `[ASSUMPTION]`; mark unknowns `UNCONFIRMED`.
- Persist task continuity in Beads rather than `.agent/STATE.md`. Beads files stay out of git (stealth mode).
## Beads and Herdr working conventions

Beads (`bd`) is the canonical task state, shared by every agent running concurrently. Herdr is the execution environment. There is no separate board: `herdr-beads` renders `bd` inside Herdr. `BEADS_DIR` points at the backend database, so `bd` works from any directory, including worktrees.

- A task is a bead. When a prompt names one (`backend-xxxx`), run `bd show <id>` before changing anything, then `bd update <id> --claim`. For substantive work without a bead, `bd search "<keywords>"` (add `--status all` for closed ones) or `bd create "<title>" -t task -p 2` first. Never keep a markdown TODO list instead.
- Keep the bead current: `bd note <id> "<finding, decision, or blocker>"` for durable context; `bd create ... --parent <id>` or `bd dep add <blocked> <blocker>` for discovered work; `bd close <id> --reason "<what shipped and how it was verified>"` when done. `bd prime` prints the full workflow.
- Statuses are the board. `open` is the backlog and the label `next` marks what is selected; `in_progress` with your assignee is working; `blocked` is externally blocked, with the blocker and how to check it in a note; `needs_me` means a human decision is required, so put the exact question and options in a note, `bd update <id> --status needs_me`, and stop; `closed` is done.
- Running, blocked, and done agent states come from Herdr. When `HERDR_ENV=1`, run `herdr agent rename "$HERDR_PANE_ID" <bead-id>` as soon as you know the bead so the agent panel shows it.
- Beads files are excluded from git by stealth mode. Never `bd dolt push`, commit, or push unless asked.

## Linear work tracking

Linear is the team-visible record of work; Beads is agent-local continuity. Track substantive work in both: the bead holds working notes, the Linear issue holds status and progress others read. This section authorizes the Linear writes it describes.

- Team is **Infra** (`INF-123` keys). Assign issues to me. Skip Linear for questions, reviews, and one-off lookups.
- Before creating, search Infra for an existing issue (`list_issues` with a `query`) and reuse it. Put the Linear key in the bead title or a note; keep bead ids out of Linear, since teammates cannot see them.
- Size the structure to the work:
  - One PR or a small change: a single issue.
  - Large work with separable parts: a parent issue with a sub-issue per independently shippable piece (usually one PR each).
  - Epics are Linear **projects** under Infra (`P-INF-…`), holding the parent issues. Create one only when I say to; otherwise attach issues to an existing project when one clearly fits, and ask if unsure.
- Keep status current: `In Progress` when work starts, `In Review` when the PR is up, `Done` when it merges and is verified, `Canceled` when dropped. Don't fight the GitHub integration when it moves status itself. Close a parent only when its sub-issues are done or canceled.
- Comment on the issue at meaningful milestones: a decision, a blocker (and what unblocks it), a scope change, or the verified outcome. No play-by-play; one concise comment per milestone.
- Newly discovered work becomes a sub-issue or a linked issue, not a line buried in a comment.
- Pull requests: link the PR from the issue and the issue from the PR, and prefix the title with the key: `INF-123: Imperative description`. Never invent a key.

## Pull requests

- Link PRs to Linear per the Linear work tracking section.
- Do not mention Codex or similar tooling in PR-visible titles, descriptions, comments, labels, branch names, or commit-attribution notes.
- Follow the repository's PR template and keep the title and description current with the final changes.
- Assign new GitHub pull requests to `travisjeffery`.

## Completion

- A task is complete when the requested change is implemented or the question is answered, proportionate verification is reported, and the impact is explained.
- List anything intentionally left out, any unresolved warnings or failures, and concrete follow-up work.
- Update Beads when the work materially changes goals, decisions, progress, discoveries, outcomes, or follow-up scope.

## Context7 MCP (library docs)

Use Context7 to fetch accurate, version-matched documentation during coding tasks.

- Add `use context7` when you need library/API docs.
- If known, pin the library with slash syntax (e.g., `use library /supabase/supabase`).
- Mention the target version.
- Fetch minimal targeted docs; summarize (no large dumps).
