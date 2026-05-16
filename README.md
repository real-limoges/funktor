# funktor

An interactive music application written in Haskell. Funktor models musical concepts — pitch, rhythm, patterns — using a strongly-typed, compositional approach, with the long-term goal of jazz harmony exploration and live coding.

## Status

Early development, but every named module has a real implementation — nothing
in `src/` is `undefined`. Pattern DSL, scheduler, harmony, generative sources,
MIDI input, Launchpad grid binding, recording, and the ASCII TUI all work
end-to-end against a SuperCollider audio backend (`scsynth`).

**What's working:**

- Musical primitives: `Beat` (Rational), `Arc`, `Event { whole, part, value }`, `Pitch`, `Velocity`, `Duration`, `Tempo`, `Note`
- Chord/scale types: `ChordSymbol`, `ChordQuality`, `Scale`, `ScaleDegree`
- Streams as `Arc -> [Event a]` queries: `silence`, `periodic`, `fromList`, `singleton`, `cat`, `stack`, `slow`, `fast`, `shiftStream`, `merge`, `mergeMany`, `sometimes`, `everyN`, `pentatonic`
- Harmony: `qualityIntervals`, `chordTones`, `scaleTones`; voicing (`bestVoicing`, `voiceLead`, Drop2/Drop3); jazz-scale analysis (`jazzScales`, `scalesForChord`, `chordsFromScale`, `classifyIntervals`)
- Generative: Euclidean rhythms (`bjorklund`, `euclidean`, `polyEuclidean`), weighted Markov chains (`runChain`, `jazzBluesChain`), Wolfram cellular automata (`rule30`/`rule90`/`rule110`, `caStream`, `caRhythm`, `caSequence`)
- Grid model: `Grid`, `Pad`, `Color`, `PadAction`, `setPad`, `getPad`
- Grid binding: `Sequencer` / `Instrument` / `Scene` modes via `pressPad` / `releasePad` and `setGridMode`
- Audio backend: `hosc` OSC client to SuperCollider's `scsynth` over UDP (synthesis, voice pool, envelopes, and effects all run in SC)
- Scheduler: wall-clock-driven event scheduler (`GHC.Clock`-based) bridging `Stream Note` to OSC `/s_new` / `/n_set` / `/n_free` calls; `hotSwap` + `enqueueImmediate` exposed as public `STM` actions
- GHCi live interface: `play`, `stop`, `reload`, `setTempo` with atomic stream hot-swap; session survives `:reload` via `foreign-store`; `fsnotify` watcher auto-prompts on `.hs` saves
- MIDI input: PortMidi-backed device enumeration, note-in / CC-in / sysex-out, background reassembly thread, scheduler wiring (`startMidi` / `stopMidi`)
- Launchpad: Mk3 SysEx + grid binding (Sequencer / Instrument / Scene modes), router thread, full 9x9 surface (8x8 grid + top control row + right control column)
- Session recording: `startRecording` / `recordEvent` / `materializeSession` / `exportMidi` (Type-0 SMF via a hand-rolled `Data.ByteString.Builder` encoder)
- TUI: pure `applyEvent` reducer + `renderUI` ASCII layout, driven by `runUI` over the scheduler `TVar`

See `docs/architecture.md` for the module-level inventory and `docs/state-and-roadmap.md` for the full feature breakdown plus what's deliberately out of scope.

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
      Types.hs                     -- Beat/Arc/Event/Pitch + chord/scale types
      Stream.hs                    -- Arc -> [Event a] query model + smart ctors
    Audio.hs                       -- Thin facade (openDevice/noteOn/noteOff)
    Audio/
      SC.hs                        -- hosc OSC client to scsynth (UDP)
      Scheduler.hs                 -- Wall-clock event scheduler + hotSwap
      Timbre.hs                    -- SynthDef name + override params
    Harmony.hs                     -- Chord/scale interval math
    Harmony/
      Voicing.hs                   -- Inversions, drops, voice leading
      Analysis.hs                  -- Jazz scales + chord/scale matching
    Generative/
      Euclidean.hs                 -- Bjorklund pulse distribution
      Markov.hs                    -- Weighted transition chains
      CellularAutomata.hs          -- Wolfram rules + pattern shapers
    Grid.hs                        -- Pad grid model
    Grid/
      Binding.hs                   -- Sequencer / Instrument / Scene dispatcher
    Hardware/
      MIDI.hs                      -- PortMidi I/O + router thread     (midi flag)
      Launchpad.hs                 -- Launchpad Mk3 SysEx + translation (midi flag)
    Live.hs                        -- GHCi live interface
    Live/
      Reload.hs                    -- fsnotify watcher + foreign-store persistence
      Session.hs                   -- Recording + Type-0 SMF export
    UI.hs                          -- ASCII dashboard (renderUI / runUI)
synthdefs/
  funktor.scd                      -- SuperCollider source the user evaluates once
docs/
  architecture.md                  -- Module wiring + design decisions
  state-and-roadmap.md             -- What works, what's deferred, soft spots
  api.md                           -- Deferred Fugue web-integration sketch
```

## License

BSD-3-Clause
