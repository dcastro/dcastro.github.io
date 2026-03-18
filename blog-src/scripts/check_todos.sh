#!/usr/bin/env bash

set -euo pipefail

# This script scans `lhs` files for TODOs.

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

lhs_files=()
while IFS= read -r -d '' file; do
	lhs_files+=("$file")
done < <(find "$repo_root" -type f -name '*.lhs' -print0)

if ((${#lhs_files[@]} == 0)); then
	exit 0
fi

match_output="$(grep -nH 'TODO' "${lhs_files[@]}" || true)"

if [[ -n "$match_output" ]]; then
	printf 'Found TODOs in:\n' >&2
	while IFS= read -r match_line; do
			[[ -z "$match_line" ]] && continue
		file="${match_line%%:*}"
		rest="${match_line#*:}"
		line="${rest%%:*}"
		content="${rest#*:}"
		rel_file="${file#$repo_root/}"
		printf '%s : %s : %s\n' "${rel_file:-$file}" "$line" "$content" >&2
	done <<< "$match_output"
	exit 1
fi

exit 0
