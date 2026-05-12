#!/usr/bin/env bash
# Format, lint, build (warning-free), and test the Haskell tree. Source from
# your zsh profile and invoke as `precommit-hs` before committing. Exits
# non-zero on any failure; that's your signal to fix things before committing.
#
# Steps (each runs only if the previous succeeded):
#   1. fourmolu --mode inplace on every .hs under src/ test/ app/
#   2. hlint on the same tree
#   3. cabal build with -Werror (warnings become errors)
#   4. cabal test --enable-coverage
#   5. scripts/check-coverage.sh (per-module HPC thresholds)
#
# Usage:
#   precommit-hs              # run from repo root
#   precommit-hs --check      # don't modify files; fail if fourmolu would
#                             # rewrite anything or hlint has hints
#
# Wire into zsh:
#   alias precommit-hs='~/repositories/funktor/scripts/precommit-hs.sh'

set -euo pipefail

# Locate the repo root from the script's own path so it works from any cwd.
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
cd "$REPO_ROOT"

mode="inplace"
if [[ "${1:-}" == "--check" ]]; then
    mode="check"
fi

# Bail early if the tools aren't on PATH.
for tool in fourmolu hlint cabal; do
    if ! command -v "$tool" >/dev/null 2>&1; then
        echo "ERROR: $tool not found on PATH" >&2
        exit 2
    fi
done

step() {
    echo
    echo "==> $*"
}

# shellcheck disable=SC2046,SC2086 # project paths contain no spaces
hs_files=$(find src test app -type f -name '*.hs' 2>/dev/null)
if [[ -z "$hs_files" ]]; then
    echo "No .hs files under src/ test/ app/."
    exit 0
fi
file_count=$(echo "$hs_files" | wc -l | tr -d ' ')

step "fourmolu --mode $mode ($file_count files)"
fourmolu --mode "$mode" $hs_files

step "hlint"
hlint src test app

step "cabal build (warnings → errors)"
cabal build all --ghc-options=-Werror

step "cabal test (with coverage)"
cabal test --enable-coverage --test-show-details=direct

step "coverage thresholds"
bash "$SCRIPT_DIR/check-coverage.sh"

echo
echo "All green: fourmolu + hlint + build (no warnings) + tests + coverage."
