Commit any uncommitted changes (if there are any), push the current branch to origin, and create a pull request.

Steps:
1. Check git status for any uncommitted changes (staged or unstaged)
2. If there are changes, stage all changes and create a commit with an appropriate message based on the changes
3. Push the current branch to origin (with -u if needed to set upstream)
4. Create a pull request using `gh pr create` targeting the main branch

If there are no uncommitted changes, skip the commit step and proceed with push and PR creation.
If a PR already exists for this branch, inform me instead of trying to create a new one.
