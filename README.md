# funktor

An interactive music application written in Haskell. Funktor models musical concepts — pitch, rhythm, patterns — using a strongly-typed, compositional approach, with the long-term goal of jazz harmony exploration and live coding.

## Status

Early development. Audio output works end-to-end (sine + envelope + voice pool + scheduler); harmony, generative, hardware, and live-coding branches are stubbed for future work.

**What's working:**

- Musical primitives: `Beat` (Rational), `Pitch`, `Velocity`, `Duration`, `Tempo`, `Event`, `Note`
- Chord/scale types: `ChordSymbol`, `ChordQuality`, `Scale`, `ScaleDegree`
- Pattern construction and composition: `append`, `stack`, `shift`, `scale`, `repeat_`
- Infinite streams: `fromPattern`, `fromList`, `merge`, `mergeMany`, `shiftStream`
- Harmony: `qualityIntervals`, `chordTones`, `scaleTones`
- Grid model: `Grid`, `Pad`, `Color`, `PadAction`, `setPad`, `getPad`
- Audio engine: SDL2 callback-driven sine oscillator, ADSR envelope, 8-voice polyphonic pool with oldest-steal
- Scheduler: wall-clock-driven event scheduler (`GHC.Clock`-based) bridging `Stream Note` to the voice pool
- GHCi live interface: `play`, `stop`, `setTempo` with atomic stream hot-swap
- Oscillator waveforms: Sine, Sawtooth, Square, Triangle (with PolyBLEP band-limiting)
- Effects: one-pole low-pass filter, delay with feedback (reverb structurally wired)

**Stubbed for future implementation:** Generative (Euclidean rhythms, Markov chains, cellular automata), Harmony voicing/analysis/extensions, Hardware (MIDI input, Launchpad), Grid-audio binding, Terminal UI, Hot reload, Session recording. See `docs/architecture.md` for the stub inventory.

## Building

Requires **GHC 9.12.2** and **Cabal 3.14**.

```bash
cabal build          # Build all components
cabal test           # Run the test suite (38 tests)
cabal run funktor    # Run the executable (plays A4 for 1 second)
```

For interactive use:

```bash
cabal repl
λ> :m Funktor.Live Funktor
λ> play (fromPattern $ pentatonic 4)
λ> setTempo (Tempo 160)
λ> stop
```

## Project Structure

```
src/
  Funktor.hs                       -- Top-level re-export module
  Funktor/
    Core/
      Types.hs                     -- Musical primitives and chord/scale types
      Pattern.hs                   -- Finite, composable musical sequences
      Stream.hs                    -- Infinite, query-by-time-range sequences
    Audio.hs                       -- SDL2 audio device (sine callback)
    Audio/
      State.hs                     -- AudioState/OscState records
      Oscillator.hs                -- Waveform types and PolyBLEP synthesis
      Envelope.hs                  -- Pure ADSR amplitude function
      Voice.hs                     -- Voice pool + oldest-steal allocation
      Scheduler.hs                 -- Wall-clock event scheduler
      Effects.hs                   -- Low-pass, delay, reverb structures
    Harmony.hs                     -- Chord/scale interval math
    Harmony/                       -- (stubs: Voicing, Analysis)
    Grid.hs                        -- Pad grid model
    Grid/                          -- (stub: Binding)
    Generative/                    -- (stubs: Euclidean, Markov, CellularAutomata)
    Hardware/                      -- (stubs: MIDI, Launchpad)
    Live.hs                        -- GHCi live interface (play/stop/setTempo)
    Live/                          -- (stubs: Reload, Session)
    UI.hs                          -- (types only; brick TUI is a future node)
docs/
  architecture.md                  -- Working architecture reference
```

## License

BSD-3-Clause
