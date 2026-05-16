{- | Dispatcher from Launchpad pad events to the audio engine. Three modes
share one 8x8 grid:

  * 'SequencerMode' — pads toggle steps in a looped pattern; the resulting
    'Stream' is hot-swapped into the scheduler on each toggle.
  * 'InstrumentMode' — pads play notes in real time via 'enqueueImmediate'.
  * 'SceneMode' — pads launch whole 'Stream's already stored per coordinate.

The top control row (Launchpad y == 8) is handled by the caller (the
Launchpad router in 'Funktor.Live') using 'topRowModeSwitch'.
-}
module Funktor.Grid.Binding (
    AudioEngine (..),
    newAudioEngine,
    GridMode (..),
    SequencerState (..),
    InstrumentConfig (..),
    defaultSequencerState,
    defaultInstrumentConfig,
    setMode,
    pressPad,
    releasePad,
    defaultGrid,
    sequencerStream,
    gridForMode,
    topRowModeSwitch,
    padToPitch,
    setStep,
) where

import Control.Concurrent.STM (
    TVar,
    atomically,
    modifyTVar',
    newTVarIO,
    readTVar,
    readTVarIO,
    writeTVar,
 )
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Vector qualified as V
import Funktor.Audio.Scheduler (SchedulerAction (..), SchedulerState, enqueueImmediate, hotSwap)
import Funktor.Audio.Timbre (defaultTimbre)
import Funktor.Core.Stream (Stream, periodic, silence)
import Funktor.Core.Types (
    Arc (..),
    Beat (..),
    Duration (..),
    Note (..),
    Pitch (..),
    Velocity (..),
    event,
 )
import Funktor.Grid (Color (..), Grid (..), Pad (..), PadAction (..), emptyGrid, setPad)

data AudioEngine = AudioEngine
    { schedVar :: !(TVar SchedulerState)
    , mode :: !(TVar GridMode)
    }

data GridMode
    = SequencerMode !SequencerState
    | InstrumentMode !InstrumentConfig
    | SceneMode !(Map (Int, Int) (Stream Note))

{- | An 8x8 toggle grid plus the pitch row and step-duration parameters that
turn it into a 'Stream Note'. Row 0 is the bottom row and plays 'root';
row @i@ adds @scale !! i@ semitones.
-}
data SequencerState = SequencerState
    { steps :: !(V.Vector (V.Vector Bool))
    , root :: !Pitch
    , scale :: ![Int]
    , stepDur :: !Duration
    }
    deriving (Eq, Show)

-- | Pitch geometry for 'InstrumentMode'.
data InstrumentConfig = InstrumentConfig
    { root :: !Pitch
    , step :: !Int
    -- ^ semitones between adjacent columns
    , rowStep :: !Int
    -- ^ semitones between adjacent rows
    }
    deriving (Eq, Show)

{- | All-off 8x8 sequencer, C minor pentatonic (C, D, Eb, G, Ab repeated to
fill 8 rows), one-beat steps.
-}
defaultSequencerState :: SequencerState
defaultSequencerState =
    SequencerState
        { steps = V.replicate 8 (V.replicate 8 False)
        , root = Pitch 48
        , scale = [0, 2, 3, 7, 8, 12, 14, 15]
        , stepDur = Duration 1
        }

-- | Instrument mode rooted at C2, one semitone per column, fifths per row.
defaultInstrumentConfig :: InstrumentConfig
defaultInstrumentConfig =
    InstrumentConfig
        { root = Pitch 36
        , step = 1
        , rowStep = 5
        }

newAudioEngine :: TVar SchedulerState -> IO AudioEngine
newAudioEngine sv = do
    modeVar <- newTVarIO (InstrumentMode defaultInstrumentConfig)
    pure AudioEngine{schedVar = sv, mode = modeVar}

-- | Swap the dispatch mode. LED redraw is the caller's responsibility.
setMode :: AudioEngine -> GridMode -> IO ()
setMode e m = atomically (writeTVar e.mode m)

{- | Dispatch a pad-down event according to the current 'GridMode'. In
'SequencerMode' the step toggles and the resulting stream is hot-swapped;
in 'InstrumentMode' a 'SchedNoteOn' is enqueued; in 'SceneMode' the stored
'Stream' (if any) is hot-swapped.
-}
pressPad :: Int -> Int -> AudioEngine -> Velocity -> IO ()
pressPad x y e vel = atomically $ do
    m <- readTVar e.mode
    case m of
        InstrumentMode cfg ->
            enqueueImmediate e.schedVar (SchedNoteOn (padToPitch cfg x y) vel defaultTimbre)
        SequencerMode st -> do
            let st' = setStep x y st
            modifyTVar' e.mode (const (SequencerMode st'))
            hotSwap e.schedVar (sequencerStream st')
        SceneMode m' -> case Map.lookup (x, y) m' of
            Just s -> hotSwap e.schedVar s
            Nothing -> pure ()

{- | Dispatch a pad-up event. Only 'InstrumentMode' acts on releases (sending
'SchedNoteOff'); Sequencer and Scene modes ignore them — toggles persist
and scene playback continues until another scene is launched.
-}
releasePad :: Int -> Int -> AudioEngine -> IO ()
releasePad x y e = do
    m <- readTVarIO e.mode
    case m of
        InstrumentMode cfg ->
            atomically $
                enqueueImmediate e.schedVar (SchedNoteOff (padToPitch cfg x y))
        _ -> pure ()

-- | The 8x8 musical surface of a Launchpad Mk3, blank.
defaultGrid :: Grid
defaultGrid = emptyGrid 8 8

{- | Build a looping 'Stream Note' from a 'SequencerState'. Column index is
time (in 'stepDur' increments); row index is pitch (via 'root' + 'scale').
One full revolution lasts @width * stepDur@ beats.
-}
sequencerStream :: SequencerState -> Stream Note
sequencerStream st
    | V.null st.steps = silence
    | otherwise = periodic patDur evts
  where
    w = V.length (V.head st.steps)
    stepBeats = unDuration st.stepDur
    patDur = st.stepDur * Duration (fromIntegral w)
    rowPitch i = case drop i st.scale of
        offset : _ -> st.root + Pitch offset
        [] -> st.root
    evts =
        [ event (Arc s (s + Beat stepBeats)) (Note (rowPitch row) (Velocity 0.7))
        | (row, cells) <- zip [0 :: Int ..] (V.toList st.steps)
        , (col, True) <- zip [0 :: Int ..] (V.toList cells)
        , let s = Beat (fromIntegral col * stepBeats)
        ]

{- | Switch modes from a top-row press. Columns 0/1/2 select Sequencer /
Instrument / Scene; columns 3..8 are unbound for now.
-}
topRowModeSwitch :: Int -> Maybe GridMode
topRowModeSwitch col = case col of
    0 -> Just (SequencerMode defaultSequencerState)
    1 -> Just (InstrumentMode defaultInstrumentConfig)
    2 -> Just (SceneMode Map.empty)
    _ -> Nothing

{- | LED state for a mode's pads. Used to repaint the whole Launchpad on
mode change.
-}
gridForMode :: GridMode -> Grid
gridForMode m = case m of
    InstrumentMode _ ->
        (emptyGrid 8 8){pads = replicate 8 (replicate 8 (Pad NoAction Cyan))}
    SequencerMode st ->
        let toggled =
                [ (col, row)
                | (row, cells) <- zip [0 :: Int ..] (V.toList st.steps)
                , (col, True) <- zip [0 :: Int ..] (V.toList cells)
                ]
         in foldr lightYellow (emptyGrid 8 8) toggled
    SceneMode scenes ->
        foldr lightYellow (emptyGrid 8 8) (Map.keys scenes)
  where
    lightYellow (x, y) = setPad x y (Pad NoAction Yellow)

-- | Compute the pitch a pad triggers in 'InstrumentMode'.
padToPitch :: InstrumentConfig -> Int -> Int -> Pitch
padToPitch cfg x y =
    cfg.root + Pitch (x * cfg.step + y * cfg.rowStep)

{- | Flip the cell at @(x, y)@ in a 'SequencerState'. Out-of-bounds coords
are a no-op (matching 'setPad' in "Funktor.Grid").
-}
setStep :: Int -> Int -> SequencerState -> SequencerState
setStep x y st
    | y < 0 || y >= V.length st.steps = st
    | x < 0 || x >= V.length (st.steps V.! y) = st
    | otherwise =
        let row = st.steps V.! y
            row' = row V.// [(x, not (row V.! x))]
            steps' = st.steps V.// [(y, row')]
         in st{steps = steps'}
