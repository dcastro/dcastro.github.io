#!/usr/bin/env bash

set -e # exit on error
set -u # error on undefined var
set -o pipefail # exit on command pipe failure

# Set working dir to script location
cd "$(dirname "$0")"

# Hardcoded mapping of source to destination files
declare -A file_map
file_map["../blog-src/src/LiterateHaskellExample.lhs"]="../blog/_drafts/2025-11-06-example.md"
file_map["../blog-src/src/Mbc.lhs"]="../blog/_posts/2026-03-04-the-hidden-perils-of-monad-base-control.md"

for from in "${!file_map[@]}"; do
    to="${file_map[$from]}"
    echo "Converting $from -> $to"
    pandoc \
        --standalone \
        "$from" \
        -o "$to" \
        --from markdown+lhs --to gfm
done
