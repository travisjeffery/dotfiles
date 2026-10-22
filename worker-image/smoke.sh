#!/bin/sh
# Smoke test run inside the built image (CI and after a host update): the
# pinned tools are the versions the manifest says, and a Herdr server starts
# and answers its API.
set -eu

fail=0
check() { # name, expected, actual
	if [ "$2" = "$3" ]; then echo "ok   $1 $3"; else echo "FAIL $1: want $2, got $3"; fail=1; fi
}

check herdr "${HERDR_VERSION:?}" "$(herdr --version | awk '{print $2}')"
quartermaster --version >/dev/null && echo "ok   quartermaster $(quartermaster --version 2>&1 | head -1)"

# Every [tools] entry in the manifest resolves to exactly its pinned version.
mise ls --current --json | jq -r 'to_entries[] | .key as $k | .value[] | "\($k) \(.requested_version) \(.version) \(.installed)"' |
while read -r tool want got installed; do
	[ "$installed" = true ] || { echo "FAIL $tool not installed"; echo 1 >/tmp/smoke-fail; continue; }
	[ "$want" = "$got" ] && echo "ok   $tool $got" || { echo "FAIL $tool: want $want, got $got"; echo 1 >/tmp/smoke-fail; }
done
[ -e /tmp/smoke-fail ] && fail=1

for c in claude codex gh go node gopls golangci-lint git ssh; do
	command -v "$c" >/dev/null && echo "ok   $c on PATH" || { echo "FAIL $c not on PATH"; fail=1; }
done

# A Herdr server starts headless and answers agent list.
herdr server >/tmp/herdr-server.log 2>&1 &
pid=$!
i=0
until herdr agent list >/dev/null 2>&1; do
	i=$((i + 1))
	if [ "$i" -gt 30 ]; then echo "FAIL herdr server did not answer"; cat /tmp/herdr-server.log; fail=1; break; fi
	sleep 1
done
[ "$i" -le 30 ] && echo "ok   herdr server answers agent list"
herdr server stop >/dev/null 2>&1 || kill "$pid" 2>/dev/null || true

exit "$fail"
