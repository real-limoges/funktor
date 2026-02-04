# Speculative Ideas

Open design questions, alternative approaches, and ideas to revisit as the project evolves.

---

## Envelope: Function vs State Machine

An ADSR envelope could be modeled as:

- **Pure function** `Time -> Amplitude` — composable, easy to test, but harder to ask "is this note finished?"
- **State machine** with explicit phases (Attack, Decay, Sustain, Release) — easier to query lifecycle, trickier to compose

The state machine needs to track both note-on time and note-off time, since release can happen during any phase.

## Voice Stealing Strategies

Currently planned: steal the oldest voice when max polyphony is exceeded. Other options worth revisiting:

- **Steal quietest** — less audible disruption
- **Same-pitch retrigger** — better for jazz voicings where you re-voice the same note
- **Refuse new note** — simplest, but musically frustrating

Same-pitch retrigger becomes more relevant once chord voicings are in play (Phase 6).

## Pattern Stacking: Alternative Length Semantics

`stack` currently uses max duration (shorter pattern gets trailing silence). Two other models:

- **Min duration** — truncate the longer pattern. Useful for "play only as long as X."
- **LCM duration** — both patterns loop until they realign. Creates polyrhythmic textures (e.g., a 3-beat pattern against a 4-beat pattern produces a 12-beat cycle).

LCM stacking could be a separate combinator rather than changing `stack`.

## Chord Representation: The Hybrid Model

The initial plan is enumerated chord qualities (`Maj7`, `Min7`, `Dom7`, ...). When the combinatorial explosion hits (Maj7#11, Min9, Alt, etc.), refactor to a hybrid:

```haskell
data ChordSymbol = ChordSymbol
    { chordRoot :: Pitch
    , chordBase :: BaseQuality  -- Maj, Min, Dom, Dim, Aug
    , chordExtensions :: [Extension]  -- 7, 9, 11, 13, and alterations
    }
```

This preserves lead-sheet vocabulary ("Dm7") while supporting arbitrary extensions.

## Hot Reload Approaches

`foreign-store` is the current plan for persisting state across GHCi reloads. Alternatives if that proves too hacky:

- **`hint` library** — interpret Haskell at runtime, full flexibility but adds a dependency and complexity
- **File watcher + custom protocol** — watch source files, reload on save, communicate changes over a channel. More infrastructure but decouples reload from GHCi.

## Audio Backend: Callback vs Queue

Currently leaning queue-based (push samples, SDL plays them) for simplicity. Callback-based (SDL calls your function when it needs samples) offers lower latency but requires careful state management — the callback runs on a dedicated thread and must not block or allocate.

Switch to callback if latency from queue-based output becomes a problem.
