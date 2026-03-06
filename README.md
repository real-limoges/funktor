# funktor

An interactive music application written in Haskell. Funktor models musical concepts — pitch, rhythm, patterns — using a strongly-typed, compositional approach, with the long-term goal of jazz harmony exploration and live coding.

## Status

Early development. Core types, pattern composition, streams, harmony, grid model, and audio device are implemented; no sound output yet.

**What's working:**

- Musical primitives: `Beat`, `Pitch`, `Velocity`, `Duration`, `Tempo`, `Event`, `Note`
- Chord/scale types: `ChordSymbol`, `ChordQuality`, `Scale`, `ScaleDegree`
- Rational-based beat positions for exact arithmetic (no floating-point drift)
- Pattern construction and composition: sequential (`append`), parallel (`stack`), `shift`, `scale`, `repeat_`
- Infinite streams: `fromPattern`, `fromList`, `merge`, `mergeMany`, `shiftStream`
- Harmony: `qualityIntervals`, `chordTones`, `scaleTones` for all chord qualities
- Grid model: `Color`, `PadAction`, `Pad`, `Grid` with `emptyGrid`, `setPad`, `getPad`
- Audio device: SDL2 callback-based device opens and plays silence (milestone A1)
- Pitch/frequency conversion (`midiToFreq`, `freqToMidi`)

## Building

Requires **GHC 9.12.2** and **Cabal 3.14**.

```bash
cabal build          # Build all components
cabal test           # Run the test suite
cabal run funktor    # Run the executable
```

## Project Structure

```
src/
  Funktor.hs                 -- Top-level re-export module
  Funktor/
    Core/
      Types.hs               -- Musical primitives and chord/scale types
      Pattern.hs             -- Finite, composable musical sequences
      Stream.hs              -- Infinite, query-by-time-range sequences
    Audio.hs                 -- SDL2 audio device (callback approach)
    Harmony.hs               -- Chord tones, scale tones, quality intervals
    Grid.hs                  -- Pad grid model for controller interfaces
docs/
  ROADMAP.md                 -- Skill-tree implementation guide (non-linear)
  IMPL_GUIDE.md              -- Detailed implementation notes per node
  THEORY.md                  -- Concepts and theory behind the design
  SPECULATIVE.md             -- Open design questions
  LAUNCHPAD_GUIDE.md         -- Launchpad hardware integration notes
```

## License

BSD-3-Clause