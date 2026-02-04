# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Funktor is an interactive music application written in Haskell. It models musical concepts (pitch, rhythm, patterns) using a strongly-typed, compositional approach. Currently in early development — core types and pattern composition are implemented; no audio output yet.

## Build Commands

```bash
cabal build          # Build all components
cabal run funktor    # Run the executable
cabal test           # Run the test suite
cabal clean          # Clean build artifacts
```

The project uses **GHC 9.12.2** with the **GHC2024** language standard and **Cabal 3.14**. There are no external dependencies beyond `base`.

## Architecture

The library follows a layered module structure:

- **`Funktor`** — Top-level re-export module.
- **`Funktor.Core.Types`** — Musical primitives: `Beat` (Rational-based time), `Pitch` (MIDI int), `Velocity`, `Duration`, `Tempo`, `Event a`, and `Note`. Uses newtype wrappers extensively for type safety. Rational arithmetic for beat positions avoids floating-point drift over long compositions.
- **`Funktor.Core.Pattern`** — Finite, repeatable musical sequences (`Pattern a`). Patterns are immutable and compose via `append` (sequential), `stack` (parallel), `shift`, `scale`, and `repeat_`. Events are auto-sorted on construction. Duration is tracked explicitly to enable sequencing.

Both `Event` and `Pattern` are `Functor` instances.

## Key Design Decisions

- **Rational time**: Beat positions use `Rational` (not `Double`) for exact arithmetic — this is intentional and important.
- **Strict fields**: Pattern internals use `!` for strict evaluation.
- **No external deps**: The library intentionally depends only on `base`.
- **`RecordWildCards`** extension is enabled project-wide.

## GHC Warnings

The project enables strict warnings: `-Wall -Wcompat -Widentities -Wincomplete-record-updates -Wincomplete-uni-patterns -Wpartial-fields -Wredundant-constraints`. All code should compile warning-free.

## Roadmap

See `ROADMAP.md` for the detailed 9-phase development plan covering audio output, envelopes, scheduling, jazz harmony, generative elements, MIDI hardware support, and live coding.