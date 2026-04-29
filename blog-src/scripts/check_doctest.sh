#!/usr/bin/env bash
set -euo pipefail

: '

`doctest` will ignore any "doctests" (marked with `>>>`) that are not
part of a haddock comment (marked with `-- |`)
or a setup block (marked with `-- $setup`).

This script ensures that all doctests are properly marked with a pipe `|` to be recognized by `doctest`.



The script will flag doctests like this:

```hs
-- >>> 1 + 1
myFunc :: IO ()
```

And will allow these:

```hs
-- | >>> 1 + 1
myFunc :: IO ()
```

```hs
-- | >>> 1 + 1
-- >>> 1 + 1
myFunc :: IO ()
```

```hs
-- | Some text
-- More text
--
-- >>> 1 + 1
myFunc :: IO ()
```

```hs
-- $setup
-- >>> 1 + 1
```

'

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

while IFS= read -r -d '' file; do
  # Split the grepped line by the character ':' into line number and content
  while IFS=: read -r line match; do
    if [[ -z "${line}" ]]; then
      continue
    fi

    prev_line_number=$((line - 1))
    if (( prev_line_number >= 1 )); then
      prev_line=$(sed -n "${prev_line_number}p" "$file")

      # The pattern `-- >>>` is allowed if it's preceded by a line with `$setup`.
      if [[ $prev_line =~ ^[[:space:]]*--[[:space:]]*\$setup[[:space:]]*$ ]]; then
        continue
      fi

      # Allow `-- >>>` when it belongs to a contiguous Haddock comment block
      # that started with `-- |`.
      scan_line_number=$prev_line_number
      in_haddock_block=false
      while (( scan_line_number >= 1 )); do
        scan_line=$(sed -n "${scan_line_number}p" "$file")

        if [[ $scan_line =~ ^[[:space:]]*--[[:space:]]*\| ]]; then
          in_haddock_block=true
          break
        fi

        # Continue scanning through contiguous line comments like `--`,
        # `-- text`, or `-- >>>`.
        if [[ $scan_line =~ ^[[:space:]]*--([[:space:]].*)?$ ]]; then
          scan_line_number=$((scan_line_number - 1))
          continue
        fi

        break
      done

      if [[ $in_haddock_block == true ]]; then
        continue
      fi
    fi

    echo "Invalid doctest marker in $file:" >&2
    echo "${line}:${match}" >&2
    echo "This test will be ignored by doctest, replace with \`-- | >>>\`" >&2
    exit 1
    # Scan for lines starting with `-- >>>`
  done < <(grep -nE '^[[:space:]]*--[[:space:]]+>>>' "$file" || true)
# Scan .hs files
done < <(find "$repo_root" -name '*.lhs' -print0)

exit 0
