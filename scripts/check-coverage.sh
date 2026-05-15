#!/usr/bin/env bash
# Parse `hpc report --per-module` output and enforce per-module thresholds.
# Run after `cabal test --enable-coverage`. Exits non-zero if any gated
# module is below its target expression-coverage percentage.

set -euo pipefail

TIX=$(find dist-newstyle -name "funktor-test.tix" 2>/dev/null | head -1)
# Scope to funktor-* so library dependencies (e.g. vendored PortMidi) that
# also produce hpc artifacts don't shadow our own mix directory.
LIBMIX=$(find dist-newstyle -type d -path "*funktor-0*/build/extra-compilation-artifacts/hpc/vanilla/mix" 2>/dev/null | head -1)
TESTMIX=$(find dist-newstyle -type d -path "*funktor-test-tmp/extra-compilation-artifacts/hpc/vanilla/mix" 2>/dev/null | head -1)
# Library deps compiled with -fhpc need their mix dirs registered too, even
# though we don't gate on their coverage — hpc otherwise fails to map the tix
# entries it finds for them.
DEPMIXES=$(find dist-newstyle -type d -path "*build/extra-compilation-artifacts/hpc/vanilla/mix" -not -path "*funktor-0*" 2>/dev/null)

if [[ -z "$TIX" || -z "$LIBMIX" ]]; then
    echo "ERROR: no coverage artifacts found. Run 'cabal test --enable-coverage' first." >&2
    exit 2
fi

DEPMIX_ARGS=()
while IFS= read -r dir; do
    [[ -n "$dir" ]] && DEPMIX_ARGS+=("--hpcdir=$dir")
done <<<"$DEPMIXES"

REPORT=$(hpc report "$TIX" \
    --hpcdir="$LIBMIX" \
    --hpcdir="$TESTMIX" \
    "${DEPMIX_ARGS[@]}" \
    --exclude=Main \
    --exclude=Paths_funktor \
    --per-module)

# Module -> minimum expression coverage %.
# Tuned to current coverage with ~5% regression margin. Stubbed modules are
# intentionally omitted; their bodies are `undefined` so coverage is meaningless.
declare -a THRESHOLDS=(
    "Funktor.Audio:50"
    "Funktor.Audio.Effects:55"
    "Funktor.Audio.Envelope:95"
    "Funktor.Audio.Oscillator:90"
    "Funktor.Audio.Scheduler:5"
    "Funktor.Audio.State:85"
    "Funktor.Audio.Voice:85"
    "Funktor.Core.Pattern:75"
    "Funktor.Core.Stream:78"
    "Funktor.Core.Types:80"
    "Funktor.Grid:90"
    "Funktor.Hardware.MIDI:40"
    "Funktor.Harmony:95"
)

fail=0
for entry in "${THRESHOLDS[@]}"; do
    mod="${entry%:*}"
    target="${entry##*:}"
    line=$(echo "$REPORT" | grep -A1 "<module funktor-.*-inplace/${mod}>" | tail -1 || true)
    if [[ -z "$line" ]]; then
        echo "MISSING  $mod (no report row)"
        fail=1
        continue
    fi
    pct=$(echo "$line" | sed -E 's/[^0-9]*([0-9]+)%.*/\1/')
    if [[ -z "$pct" ]]; then
        echo "PARSE    $mod (cannot extract %)"
        fail=1
        continue
    fi
    if (( pct < target )); then
        printf 'FAIL     %-32s %3d%% < %3d%%\n' "$mod" "$pct" "$target"
        fail=1
    else
        printf 'OK       %-32s %3d%% >= %3d%%\n' "$mod" "$pct" "$target"
    fi
done

if (( fail )); then
    echo
    echo "Coverage below threshold for one or more modules." >&2
    exit 1
fi
echo "All gated modules meet coverage targets."
