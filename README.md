# funktor

An interactive music application written in Haskell. Funktor models musical concepts — pitch, rhythm, patterns — using a strongly-typed, compositional approach, with the long-term goal of jazz harmony exploration and live coding.

## Status

Early development. Core types and pattern composition are implemented; no audio output yet.

**What's working:**

- Musical primitives: `Beat`, `Pitch`, `Velocity`, `Duration`, `Tempo`, `Event`, `Note`
- Rational-based beat positions for exact arithmetic (no floating-point drift)
- Pattern construction and composition: sequential (`append`), parallel (`stack`), `shift`, `scale`, `repeat_`
- Pitch/frequency conversion (`midiToFreq`, `freqToMidi`)

## Building

Requires **GHC 9.12.2** and **Cabal 3.14**. No external dependencies beyond `base`.

```bash
cabal build          # Build all components
cabal test           # Run the test suite
cabal run funktor    # Run the executable
```

## Project Structure

```
src/
  Funktor.hs                 -- Top-level re-export module
  Funktor/Core/
    Types.hs                 -- Musical primitives
    Pattern.hs               -- Finite, composable musical sequences
docs/
  ROADMAP.md                 -- Development plan
  SPECULATIVE.md             -- Open design questions
```

## License

BSD-3-Clause
