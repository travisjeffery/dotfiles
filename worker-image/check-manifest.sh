#!/bin/sh
# Fails if the worker image manifest has a tool that isn't pinned to an exact
# version ("latest", empty, or a prefix like "1.26").
set -eu
manifest="${1:-$(dirname "$0")/mise.toml}"
bad=$(awk '
	/^\[/ { in_tools = ($0 == "[tools]"); next }
	in_tools && /=/ && !/^[[:space:]]*#/ {
		v = $0; sub(/^[^=]*=[[:space:]]*/, "", v); gsub(/"/, "", v)
		if (v == "latest" || v == "" || v !~ /^[0-9]+\.[0-9]+\.[0-9]+/) print
	}' "$manifest")
if [ -n "$bad" ]; then
	echo "unpinned tools in $manifest:" >&2
	echo "$bad" >&2
	exit 1
fi
echo "manifest pinned: $manifest"
