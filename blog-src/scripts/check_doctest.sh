#!/usr/bin/env bash
set -euo pipefail

: '

`doctest` will ignore any "doctests" (marked with `>>>`) that are not
part of a haddock comment (marked with `-- |`)
or a haddock block comment (marked with `{- |`)
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

```hs
{- | Some text

>>> 1 + 1
-}
```

'

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

while IFS= read -r -d '' file; do

  # 1st check: Lines starting with `-- >>>` must be preceded by a line with `-- |` or `$setup`.

  # Split the grepped line by the character ':' into line number and content
  while IFS=: read -r line match; do
    if [[ -z "${line}" ]]; then
      continue
    fi

    prev_line_number=$((line - 1))
    if (( prev_line_number >= 1 )); then
      prev_line=$(sed -n "${prev_line_number}p" "$file")

      # Allow `-- >>>` when it belongs to a contiguous Haddock comment block
      # that started with `-- |` or `-- $setup`.
      scan_line_number=$prev_line_number
      in_setup_block=false
      in_haddock_block=false
      while (( scan_line_number >= 1 )); do
        scan_line=$(sed -n "${scan_line_number}p" "$file")

        if [[ $scan_line =~ ^[[:space:]]*--[[:space:]]*\$setup[[:space:]]*$ ]]; then
          in_setup_block=true
          break
        fi

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

      if [[ $in_setup_block == true ]]; then
        continue
      fi

      if [[ $in_haddock_block == true ]]; then
        continue
      fi
    fi

    echo "Invalid doctest marker in $file:" >&2
    echo "${line}:${match}" >&2
    echo 'This test will be ignored by doctest; ensure it is inside Haddock docs (`-- |` or `{- |`).' >&2
    exit 1
    # Scan for lines starting with `-- >>>`
  done < <(grep -nE '^[[:space:]]*--[[:space:]]+>>>' "$file" || true)

  # 2nd check: Lines starting with `>>>` must be inside a block comment that starts with `{- |`.

  # Split the grepped line by the character ':' into line number and content.
  while IFS=: read -r line match; do
    if [[ -z "${line}" ]]; then
      continue
    fi

    # Allow plain `>>>` only when it appears inside a Haddock block comment
    # that starts with `{- |`.
    scan_line_number=$line
    in_block_comment=false
    in_haddock_block=false
    while (( scan_line_number >= 1 )); do
      scan_line=$(sed -n "${scan_line_number}p" "$file")

      # If we hit a block-comment close while scanning upward, this `>>>` is
      # not inside that block comment.
      if [[ $scan_line =~ -\} ]]; then
        break
      fi

      if [[ $scan_line =~ \{-[[:space:]]*\| ]]; then
        in_block_comment=true
        in_haddock_block=true
        break
      fi

      if [[ $scan_line =~ \{- ]]; then
        in_block_comment=true
        break
      fi

      scan_line_number=$((scan_line_number - 1))
    done

    if [[ $in_block_comment == true && $in_haddock_block == true ]]; then
      continue
    fi

    echo "Invalid doctest marker in $file:" >&2
    echo "${line}:${match}" >&2
    echo 'This test will be ignored by doctest; ensure it is inside Haddock docs (`-- |` or `{- |`).' >&2
    exit 1
    # Scan for plain doctest prompts like `>>> 1 + 1`.
  done < <(grep -nE '^[[:space:]]*>>>[[:space:]]+' "$file" || true)
# Scan .hs and .lhs files
done < <(find "$repo_root" \( -name '*.lhs' -o -name '*.hs' \) -print0)

exit 0
