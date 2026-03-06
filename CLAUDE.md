# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Funktor is an interactive music application written in Haskell. It models musical concepts (pitch, rhythm, patterns) using a strongly-typed, compositional approach, with the long-term goal of jazz harmony exploration and live coding.

## Build Commands

```bash
cabal build          # Build all components
cabal run funktor    # Run the executable
cabal test           # Run the test suite
cabal clean          # Clean build artifacts
```

There is no framework for running individual tests — `test/Main.hs` is a single `exitcode-stdio-1.0` entry point; add test cases there directly.

The project uses **GHC 9.12.2** with the **GHC2024** language standard and **Cabal 3.14**. Library dependencies: `base`, `sdl2 >= 2.5`, `stm >= 2.5`, `vector >= 0.12`. The executable and test suite depend only on `base` + the local `funktor` library.

## Architecture

The library is layered — each layer depends only on what's below it:

```
Funktor.Audio       Funktor.Harmony     Funktor.Grid
        \                  |                /
         \          Funktor.Core.Stream    /
          \                |             /
           \    Funktor.Core.Pattern     /
            \              |           /
             \   Funktor.Core.Types   /
              \________________________/
                        |
                    Funktor  (re-exports Types + Harmony + Grid)
```

**`Funktor.Core.Types`** — Musical primitives as newtype wrappers: `Beat` (Rational-based time), `Pitch` (MIDI int), `Velocity`, `Duration`, `Tempo`, `Event a`, `Note`. Also chord/scale types: `ChordSymbol`, `ChordQuality`, `Scale`, `ScaleDegree`. `Event` is a `Functor`.

**`Funktor.Core.Pattern`** — Finite, repeatable musical sequences (`Pattern a`). Composes via `append` (sequential), `stack` (parallel), `shift`, `scale`, `repeat_`. Events are auto-sorted on construction; duration is tracked explicitly. `Pattern` is a `Functor`.

**`Funktor.Core.Stream`** — Infinite sequences represented as `newtype Stream a = Stream { runStream :: Beat -> Beat -> [Event a] }`. Constructed with `fromPattern` (loops a pattern), `fromList` (one-shot). Transforms: `shiftStream`, `merge`, `mergeMany`. `Stream` is a `Functor`. The function-from-time-range representation is intentional — it avoids materialising infinite lists and makes windowed queries efficient.

**`Funktor.Audio`** — SDL2 audio device (`openDevice`). Uses the **callback** approach (not queue): `silenceCallback` fills buffers with zeros via GADT pattern matching on `SDL.AudioFormat`. Device opens at 44100 Hz mono float, 512-sample buffers.

**`Funktor.Harmony`** — `qualityIntervals`, `chordTones`, `scaleTones`. Covers all `ChordQuality` variants. No audio dependency — usable in GHCi standalone.

**`Funktor.Grid`** — Abstract pad grid model (`Grid`, `Pad`, `Color`, `PadAction`). `setPad` is out-of-bounds safe. No audio or hardware dependency.

## Key Design Decisions

- **Rational time**: `Beat` uses `Rational` (not `Double`) — triplets are exactly 1/3, no floating-point drift over long compositions. This is intentional; do not change to `Double`.
- **Strict fields**: `Pattern` internals use `!` for strict evaluation.
- **Stream as query function**: `Stream` is `Beat -> Beat -> [Event a]`, not an infinite list. Queries are pure and composable.
- **`RecordWildCards`** extension is enabled project-wide.

## GHC Warnings

All code must compile warning-free under: `-Wall -Wcompat -Widentities -Wincomplete-record-updates -Wincomplete-uni-patterns -Wpartial-fields -Wredundant-constraints`.

## Roadmap

See `docs/ROADMAP.md` for the skill-tree implementation guide. It is non-linear — nodes have prerequisites and unlock new nodes across five branches: Sound (A-nodes), Harmony (H-nodes), Interface (I-nodes), Generative (G-nodes), Hardware (HW-nodes), Live Coding (L-nodes). Completed nodes: TYPES, PATTERN, STREAM, A1, H1, I1.