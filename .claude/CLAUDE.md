# Global Claude Guidance

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

## CodeGraph

The `codegraph` MCP server holds a symbol and call-graph index of the repo, kept current by a SessionStart hook. Use it for code navigation before grep fan-outs or Explore agents.

- Start "where is", "who calls", "how does X flow", and architecture questions with `codegraph_context`, then `codegraph_explore` for the source that matters. Read files only for what the graph does not show.
- If `codegraph_status` reports no index or a stale one, fall back to grep and Read. Never run a full index during a task; that is a maintenance operation.