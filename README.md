# funktor

An interactive music application written in Haskell. Funktor models musical concepts — pitch, rhythm, patterns — using a strongly-typed, compositional approach, with the long-term goal of jazz harmony exploration and live coding.

## Status

Early development. Pattern DSL, scheduler, MIDI input, and Launchpad grid
binding all work end-to-end against a SuperCollider audio backend (`scsynth`).
Some `Generative.*`, `Harmony.*`, `Live.Session` modules are stubbed for
future work.

**What's working:**

- Musical primitives: `Beat` (Rational), `Pitch`, `Velocity`, `Duration`, `Tempo`, `Event`, `Note`
- Chord/scale types: `ChordSymbol`, `ChordQuality`, `Scale`, `ScaleDegree`
- Pattern construction and composition: `append`, `stack`, `shift`, `scale`, `repeat_`
- Infinite streams: `fromPattern`, `fromList`, `merge`, `mergeMany`, `shiftStream`
- Harmony: `qualityIntervals`, `chordTones`, `scaleTones`
- Grid model: `Grid`, `Pad`, `Color`, `PadAction`, `setPad`, `getPad`
- Audio backend: OSC client to SuperCollider's `scsynth` over UDP (synthesis runs in SC)
- Scheduler: wall-clock-driven event scheduler (`GHC.Clock`-based) bridging `Stream Note` to OSC `/s_new` / `/n_set` calls
- GHCi live interface: `play`, `stop`, `setTempo` with atomic stream hot-swap; session survives `:reload` via `foreign-store`
- MIDI input: PortMidi-backed device enumeration, note-in / CC-in / sysex-out, background reassembly thread, scheduler wiring (`startMidi` / `stopMidi`)
- Launchpad: Mk3 SysEx + grid binding (Sequencer / Instrument / Scene modes)

**Stubbed for future implementation:** Generative (Euclidean rhythms, Markov chains, cellular automata), Harmony voicing/analysis/extensions, Session recording. See `docs/architecture.md` for the stub inventory.

## Building

Requires **GHC 9.12.2** and **Cabal 3.14**, and **SuperCollider** (for `scsynth`).

```bash
cabal build                # Build all components
cabal test                 # Run the test suite
cabal run funktor          # Plays A4 for 1 second through scsynth
cabal run funktor --check-sc   # Probes scsynth on 127.0.0.1:57110 and exits
```

### Booting the audio backend

`scsynth` must be running before `play`. To boot it and load Funktor's
`funktor_note` SynthDef:

1. Open `synthdefs/funktor.scd` in the SuperCollider IDE.
2. Place the cursor inside the outer parens and evaluate (Cmd/Ctrl+Enter).
3. Wait for `Funktor: synthdef loaded, server ready on port 57110`.

The Haskell side talks to `scsynth` over UDP at `127.0.0.1:57110`.

For interactive use:

```bash
cabal repl
λ> :m Funktor.Live Funktor
λ> play (fromPattern $ pentatonic 4)
λ> setTempo (Tempo 160)
λ> stop
```

### MIDI input

With a session running, attach a MIDI keyboard:

```
λ> listMidiInputs              -- print devices PortMidi sees
λ> play silence                -- silent generative stream
λ> startMidi                   -- opens first input, prints device name
-- press keys, audio comes out of scsynth
λ> stopMidi                    -- (or 'stop' tears down everything)
```

PortMidi snapshots devices at process start and version 0.2 exposes no refresh
primitive, so plugging in a new device after starting GHCi won't make it
appear — restart the session.

PortMidi is pulled from a GitHub fork via `cabal.project`
(`source-repository-package`) that strips the upstream `-msse2` flag — clang
rejects it on Apple Silicon. No system-portmidi install needed. Cloud builds
that don't want the C dependency can pass `--flags=-midi` to skip PortMidi
entirely; `Funktor.Hardware.MIDI` is then excluded from the build.

## Project Structure

```
src/
  Funktor.hs                       -- Top-level re-export module
  Funktor/
    Core/
      Types.hs                     -- Musical primitives and chord/scale types
      Pattern.hs                   -- Finite, composable musical sequences
      Stream.hs                    -- Infinite, query-by-time-range sequences
    Audio.hs                       -- thin facade (openDevice/noteOn/noteOff)
    Audio/
      SC.hs                        -- OSC client to scsynth (UDP)
      Scheduler.hs                 -- Wall-clock event scheduler
      Timbre.hs                    -- SynthDef name + override params
    Harmony.hs                     -- Chord/scale interval math
    Harmony/                       -- (stubs: Voicing, Analysis)
    Grid.hs                        -- Pad grid model
    Grid/
      Binding.hs                   -- Sequencer / Instrument / Scene dispatcher
    Generative/                    -- (stubs: Euclidean, Markov, CellularAutomata)
    Hardware/
      MIDI.hs                      -- PortMidi-backed input/output + scheduler routing
      Launchpad.hs                 -- Launchpad Mk3 SysEx + pad-note translation
    Live.hs                        -- GHCi live interface (play/stop/setTempo/startMidi/startLaunchpad)
    Live/
      Reload.hs                    -- fsnotify watcher + foreign-store persistence
      Session.hs                   -- (stub)
    UI.hs                          -- Console dashboard (renderUI / runUI)
synthdefs/
  funktor.scd                      -- SuperCollider source the user evaluates once
docs/
  architecture.md                  -- Working architecture reference
```

## License

BSD-3-Clause
