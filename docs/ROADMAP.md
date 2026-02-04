# Funktor Implementation Roadmap

## Philosophy

Build the simplest thing that makes sound, then iterate. Each phase should produce something you can play with. Resist the urge to build infrastructure for features you haven't needed yet.

## Phase 1: Make Noise

**Goal:** Type something in GHCi, hear a sound.

### 1.1 Minimal Audio Output

Get SDL2 (or PortAudio) producing a continuous tone. Forget musical abstractions—just prove you can fill an audio buffer and hear it.

```haskell
-- Target API:
playTone :: Double -> Double -> IO ()  -- frequency, duration
playTone 440 1.0  -- A4 for one second
```

**Decision point:** Callback-based vs queue-based audio.
- **Callback:** SDL calls your function when it needs samples. Lower latency, trickier state management.
- **Queue:** You push samples, SDL plays them. Simpler, but latency can drift.

**Recommendation:** Start with queue-based. Switch to callback if latency bothers you.

### 1.2 Basic Oscillator

A function from phase to sample. Just sine wave first.

```haskell
type Phase = Double  -- 0 to 1
type Sample = Double -- -1 to 1

sine :: Phase -> Sample
```

**Non-obvious bit:** You need to track phase across buffer fills. If your buffer is 512 samples and you're playing 440Hz at 44100Hz sample rate, the phase increment is `440/44100 ≈ 0.00998`. After 512 samples, phase has advanced by ~5.1 cycles. Wrap it to [0,1).

### 1.3 Pitch to Frequency — DONE

Implemented in `Funktor.Core.Types` as `midiToFreq` and `freqToMidi`.

### 1.4 First Milestone

```haskell
playNote :: Pitch -> IO ()
playNote 60  -- Middle C, fixed duration
```

You should be able to call this repeatedly and hear different pitches.

## Phase 2: Shape the Sound

**Goal:** Notes that start and stop naturally, not just abrupt on/off.

### 2.1 Envelope

An envelope maps time-since-note-start to amplitude. ADSR is the classic model:

```
     /\
    /  \____
   /        \
  /          \
 A  D   S   R
```

The envelope needs to handle note-off (release) at arbitrary times—it must know when the note was released, not just when it started.

### 2.2 Voice = Oscillator + Envelope

A voice is one sounding note. It has:
- Frequency (from pitch)
- Envelope state (time since note-on, maybe time since note-off)
- Phase (for oscillator continuity)

```haskell
data Voice = Voice
    { vFreq      :: Double
    , vPhase     :: Double
    , vStartTime :: Double
    , vReleaseTime :: Maybe Double
    , vEnvelope  :: Envelope
    }
```

### 2.3 Polyphony

Multiple voices playing simultaneously. You need:
- A way to allocate voices (note on)
- A way to release voices (note off)
- Mixing (sum the samples, maybe with some headroom)
- Voice cleanup (remove voices whose envelopes are done)

**Voice stealing:** When you exceed max polyphony, steal the oldest voice. Revisit with "same pitch retrigger" when jazz voicings are in play.

### 2.4 Second Milestone

```haskell
noteOn :: Pitch -> Velocity -> IO VoiceId
noteOff :: VoiceId -> IO ()
```

You can now play notes that fade in and out naturally. Hold a chord!

## Phase 3: Musical Time — DONE (except scheduler)

**Goal:** Think in beats, not seconds.

### 3.1–3.3 Core Types — DONE

Implemented in `Funktor.Core.Types`:
- `Beat` (Rational-based for exact subdivision — triplets are 1/3, not 0.333...)
- `Duration`, `Tempo`, `Velocity` (newtype-wrapped for type safety)
- `Event a` (polymorphic, strict fields) and `Note` (pitch + duration + velocity)
- `beatsToSeconds`, `secondsToBeats`, `velocityToAmplitude`

### 3.4 The Scheduler

This is where it gets subtle. The scheduler:
1. Knows the current beat position
2. Has a stream of upcoming events
3. Converts beat times to real times
4. Triggers events at the right moment

**The tricky part:** Audio callbacks happen at ~10ms intervals (depending on buffer size). Events need to be sample-accurate within that buffer. If a note should start 3ms into a buffer, you can't just trigger it at the buffer boundary.

**Simplification for now:** Quantize everything to buffer boundaries. A 512-sample buffer at 44100Hz is ~11.6ms. At 120 BPM, that's ~23ms per beat. So you're quantizing to roughly 1/16th note resolution. Good enough to start.

### 3.5 Third Milestone

```haskell
-- Define a pattern
let pat = [(0, Note 60 1 0.7), (1, Note 64 1 0.7), (2, Note 67 1 0.7)]
-- Play it
playPattern pat
```

Hear a C major arpeggio in time.

## Phase 4: Patterns and Streams

**Goal:** Composable musical building blocks.

### 4.1 Pattern — DONE

Implemented in `Funktor.Core.Pattern`. A finite sequence of events with a known duration. Events are auto-sorted on construction. Duration is tracked explicitly so sequential composition knows where the next pattern starts.

Construction: `empty`, `singleton`, `rest`, `note`, `notes`, `pattern_`

### 4.2 Pattern Operations — DONE

Implemented: `append` (sequential), `stack` (parallel), `shift`, `scale`, `repeat_`, `mapEvents`, `filterEvents`, plus `Functor` instance.

`stack` uses max duration (shorter pattern has trailing silence). Also includes `pentatonic` as a built-in common pattern.

### 4.3 Stream

An infinite sequence of events. The key operation is querying a time range:

```haskell
newtype Stream a = Stream (Beat -> Beat -> [Event a])

query :: Stream a -> Beat -> Beat -> [Event a]
```

### 4.4 Pattern to Stream

Looping a pattern forever:

```haskell
fromPattern :: Pattern a -> Stream a
```

The implementation is modular arithmetic on beat positions.

### 4.5 Fourth Milestone

```haskell
let bass = fromPattern bassPattern
let drums = fromPattern drumPattern
let combined = merge bass drums
-- The scheduler plays from 'combined'
```

Layered loops!

## Phase 5: The Grid

**Goal:** A virtual controller you can poke from the REPL.

### 5.1 Grid Model

```haskell
data Grid = Grid
    { gridPads :: [[Pad]]
    }

data Pad = Pad
    { padAction :: PadAction
    , padColor :: Color
    }

data PadAction
    = PlayNote Pitch Velocity
    | TriggerPattern PatternId
    | ...
```

### 5.2 Interaction

```haskell
press :: Grid -> Int -> Int -> IO ()  -- x, y
release :: Grid -> Int -> Int -> IO ()
```

For now, these just call `noteOn`/`noteOff` directly. Later, they might queue events for the scheduler.

### 5.3 Pentatonic Layout

The no-wrong-notes constraint. Map the grid to a pentatonic scale so any combination sounds good.

Minor pentatonic intervals: 0, 3, 5, 7, 10 (semitones from root)

A 4x5 grid gives you 20 pads = 4 octaves of pentatonic. Arrange so adjacent pads are musically related (up = higher octave, right = next scale degree, or whatever feels good).

### 5.4 Fifth Milestone

```haskell
let g = pentatonicGrid C
press g 0 0  -- hear a note
press g 1 0  -- different note, sounds good together
```

## Phase 6: Jazz Harmony (The Real Work Begins)

**Goal:** Represent and manipulate jazz chord concepts.

### 6.1 Chord Representation

Start with enumerated qualities (`Maj7`, `Min7`, `Dom7`, `HalfDim`, ...) for readability. Refactor to a hybrid model (base quality + extensions list) when the combinatorial explosion becomes painful.

```haskell
data ChordQuality = Maj7 | Min7 | Dom7 | HalfDim | ...
data ChordSymbol = ChordSymbol Pitch ChordQuality
```

### 6.2 Voicings

A chord symbol is abstract. A voicing is concrete—which specific pitches, in which octave, on which instrument.

```haskell
type Voicing = [Pitch]  -- Ordered from low to high

voice :: ChordSymbol -> VoicingStyle -> Voicing
```

VoicingStyle might be:
- `RootPosition` — root in bass, stack thirds
- `Drop2` — second voice from top dropped an octave
- `Rootless` — omit the root (pianist voicing, bass plays root)
- `Quartal` — stack fourths instead of thirds
- `Shell` — just 3rd and 7th

This is where the jazz learning happens. Implement each style, hear the difference.

### 6.3 Voice Leading

Given two chords, how do you move from one voicing to the next smoothly?

```haskell
voiceLead :: Voicing -> ChordSymbol -> Voicing
```

Principles:
- Minimize total movement (prefer small intervals)
- Common tones stay put
- Avoid parallel fifths/octaves (classical rule, jazz is looser)
- Guide tones (3rd and 7th) resolve by step

This is a meaty algorithm. You could spend weeks here.

### 6.4 Progressions

```haskell
type Progression = [(ChordSymbol, Duration)]

-- ii-V-I in C
twoFiveOne :: Progression
twoFiveOne =
    [ (ChordSymbol D Min7, 4)
    , (ChordSymbol G Dom7, 4)
    , (ChordSymbol C Maj7, 8)
    ]
```

### 6.5 Sixth Milestone

```haskell
let prog = twoFiveOne C
let voicings = voiceProgression Drop2 prog
playProgression voicings
```

Hear a ii-V-I with proper voice leading.

## Phase 7: Generative Elements

**Goal:** Algorithmic composition within harmonic constraints.

### 7.1 Walking Bass

Given a progression, generate a bass line that:
- Hits chord tones on strong beats
- Approaches the next chord root by step or half-step
- Has rhythmic variety (not just quarter notes)

```haskell
walkingBass :: Progression -> Stream Note
```

### 7.2 Comping Patterns

Rhythmic chord accompaniment. The "Charleston" rhythm, Bossa nova pattern, etc.

```haskell
compPattern :: CompStyle -> Progression -> Stream Note
```

### 7.3 Melodic Cells

Small melodic ideas that fit over chord changes. Licks, patterns, motifs.

```haskell
melodicCell :: Scale -> Pattern Note
applyCell :: Pattern Note -> ChordSymbol -> Pattern Note
```

### 7.4 Seventh Milestone

```haskell
let prog = fromPattern $ progression twoFiveOne
let bass = walkingBass prog
let comp = compPattern Swing prog
play $ merge bass comp
```

A rhythm section playing changes!

## Phase 8: Hardware (When You Get a Launchpad)

### 8.1 MIDI Input

Parse Launchpad button presses. The Launchpad speaks standard MIDI—note on/off messages where the note number encodes the pad position.

### 8.2 LED Feedback

Send note-on messages back to the Launchpad to light up pads. Color is encoded in velocity.

### 8.3 Bidirectional State

The grid model becomes the source of truth. Pad presses update it, and changes to it update the LEDs.

## Phase 9: Live Coding

### 9.1 Hot Reload

When you save a file, changes take effect on the next loop boundary. Likely using `foreign-store` to persist state across GHCi reloads.

### 9.2 Temporal Recursion

The Tidal/Sonic Pi approach: define what plays in terms of what's currently playing.

```haskell
every 4 (shift 0.25) $ pattern
```

"Every 4 cycles, shift the pattern by a quarter beat"

---

## Appendix: Non-Obvious Decisions

### Sample Rate

44100 Hz is standard. 48000 Hz is also common (video world). Pick one and stick with it. Don't make it configurable until you need to.

### Buffer Size

Smaller = lower latency, but more CPU overhead and risk of underruns.
- 128 samples: ~3ms latency, demanding
- 256 samples: ~6ms, reasonable
- 512 samples: ~12ms, safe default
- 1024 samples: ~23ms, noticeable lag

Start with 512.

### Timing Model

"When does beat 4.0 happen in real time?"

If tempo is constant, it's simple math. If tempo changes, you need to track tempo change events and integrate.

For now: constant tempo. Add tempo changes when you want them.

### Thread Model

Audio callbacks run on a dedicated thread, often with real-time priority. Don't block it. Don't allocate in it (GC pause = audio glitch).

Pattern: Audio thread reads from a lock-free queue. Main thread writes events to the queue. STM's `TQueue` or `TBQueue` works.

### Floating Point Precision

Phase accumulation over long periods can drift. If you're playing for an hour at 440Hz, 44100Hz sample rate, that's 158,760,000 phase increments. Float32 loses precision; Float64 is fine.

Use `Double` for phase. Don't worry about it until you've been playing for hours and notice drift.
