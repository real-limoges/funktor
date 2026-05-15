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
    -- * Engine handle
    AudioEngine (..),
    newAudioEngine,

    -- * Modes
    GridMode (..),
    SequencerState (..),
    InstrumentConfig (..),
    defaultSequencerState,
    defaultInstrumentConfig,
    setMode,

    -- * Pad event handlers
    pressPad,
    releasePad,

    -- * Pure helpers
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
import Funktor.Audio.State (AudioState)
import Funktor.Core.Pattern (Pattern (..))
import Funktor.Core.Stream (Stream, fromPattern, silence)
import Funktor.Core.Types (
    Beat (..),
    Duration (..),
    Event (..),
    Note (..),
    Pitch (..),
    Velocity (..),
 )
import Funktor.Grid (Color (..), Grid (..), Pad (..), PadAction (..), emptyGrid, setPad)

-- ---------------------------------------------------------------------------
-- Engine handle + modes
-- ---------------------------------------------------------------------------

-- | The audio engine handles a 'Grid.Binding' touches.
data AudioEngine = AudioEngine
    { engineAudioVar :: !(TVar AudioState)
    , engineSchedVar :: !(TVar SchedulerState)
    , engineMode :: !(TVar GridMode)
    }

-- | The active dispatch mode for an 'AudioEngine'.
data GridMode
    = SequencerMode !SequencerState
    | InstrumentMode !InstrumentConfig
    | SceneMode !(Map (Int, Int) (Stream Note))

{- | An 8x8 toggle grid plus the pitch row and step-duration parameters that
turn it into a 'Stream Note'. Row 0 is the bottom row and plays 'seqRoot';
row @i@ adds @seqScale !! i@ semitones.
-}
data SequencerState = SequencerState
    { seqSteps :: !(V.Vector (V.Vector Bool))
    , seqRoot :: !Pitch
    , seqScale :: ![Int]
    , seqStepDur :: !Duration
    }
    deriving (Eq, Show)

-- | Pitch geometry for 'InstrumentMode'.
data InstrumentConfig = InstrumentConfig
    { instRoot :: !Pitch
    , instStep :: !Int
    -- ^ semitones between adjacent columns
    , instRowStep :: !Int
    -- ^ semitones between adjacent rows
    }
    deriving (Eq, Show)

{- | All-off 8x8 sequencer, C minor pentatonic (C, D, Eb, G, Ab repeated to
fill 8 rows), one-beat steps.
-}
defaultSequencerState :: SequencerState
defaultSequencerState =
    SequencerState
        { seqSteps = V.replicate 8 (V.replicate 8 False)
        , seqRoot = Pitch 48
        , seqScale = [0, 2, 3, 7, 8, 12, 14, 15]
        , seqStepDur = Duration 1
        }

-- | Instrument mode rooted at C2, one semitone per column, fifths per row.
defaultInstrumentConfig :: InstrumentConfig
defaultInstrumentConfig =
    InstrumentConfig
        { instRoot = Pitch 36
        , instStep = 1
        , instRowStep = 5
        }

-- ---------------------------------------------------------------------------
-- IO surface
-- ---------------------------------------------------------------------------

newAudioEngine :: TVar AudioState -> TVar SchedulerState -> IO AudioEngine
newAudioEngine audioVar schedVar = do
    modeVar <- newTVarIO (InstrumentMode defaultInstrumentConfig)
    pure
        AudioEngine
            { engineAudioVar = audioVar
            , engineSchedVar = schedVar
            , engineMode = modeVar
            }

-- | Swap the dispatch mode. LED redraw is the caller's responsibility.
setMode :: AudioEngine -> GridMode -> IO ()
setMode e m = atomically (writeTVar (engineMode e) m)

{- | Dispatch a pad-down event according to the current 'GridMode'. In
'SequencerMode' the step toggles and the resulting stream is hot-swapped;
in 'InstrumentMode' a 'SchedNoteOn' is enqueued; in 'SceneMode' the stored
'Stream' (if any) is hot-swapped.
-}
pressPad :: Int -> Int -> AudioEngine -> Velocity -> IO ()
pressPad x y e vel = atomically $ do
    mode <- readTVar (engineMode e)
    case mode of
        InstrumentMode cfg ->
            enqueueImmediate (engineSchedVar e) (SchedNoteOn (padToPitch cfg x y) vel)
        SequencerMode st -> do
            let st' = setStep x y st
            modifyTVar' (engineMode e) (const (SequencerMode st'))
            hotSwap (engineSchedVar e) (sequencerStream st')
        SceneMode m -> case Map.lookup (x, y) m of
            Just s -> hotSwap (engineSchedVar e) s
            Nothing -> pure ()

{- | Dispatch a pad-up event. Only 'InstrumentMode' acts on releases (sending
'SchedNoteOff'); Sequencer and Scene modes ignore them — toggles persist
and scene playback continues until another scene is launched.
-}
releasePad :: Int -> Int -> AudioEngine -> IO ()
releasePad x y e = do
    mode <- readTVarIO (engineMode e)
    case mode of
        InstrumentMode cfg ->
            atomically $
                enqueueImmediate (engineSchedVar e) (SchedNoteOff (padToPitch cfg x y))
        _ -> pure ()

-- ---------------------------------------------------------------------------
-- Pure helpers
-- ---------------------------------------------------------------------------

-- | The 8x8 musical surface of a Launchpad Mk3, blank.
defaultGrid :: Grid
defaultGrid = emptyGrid 8 8

{- | Build a looping 'Stream Note' from a 'SequencerState'. Column index is
time (in 'seqStepDur' increments); row index is pitch (via 'seqRoot' +
'seqScale'). One full revolution lasts @width * seqStepDur@ beats.
-}
sequencerStream :: SequencerState -> Stream Note
sequencerStream st
    | V.null (seqSteps st) = silence
    | otherwise = fromPattern (Pattern evts patDur)
  where
    width = V.length (V.head (seqSteps st))
    stepBeats = unDuration (seqStepDur st)
    patDur = seqStepDur st * Duration (fromIntegral width)
    rowPitch i = case drop i (seqScale st) of
        offset : _ -> seqRoot st + Pitch offset
        [] -> seqRoot st
    evts =
        [ Event (Beat (fromIntegral col * stepBeats)) (Note (rowPitch row) (seqStepDur st) (Velocity 0.7))
        | (row, cells) <- zip [0 :: Int ..] (V.toList (seqSteps st))
        , (col, True) <- zip [0 :: Int ..] (V.toList cells)
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

{- | LED state for a mode's pads.  Used to repaint the whole Launchpad on
mode change.
-}
gridForMode :: GridMode -> Grid
gridForMode mode = case mode of
    InstrumentMode _ -> paintAll Cyan
    SequencerMode st ->
        let g0 = emptyGrid 8 8
            toggled =
                [ (col, row)
                | (row, cells) <- zip [0 :: Int ..] (V.toList (seqSteps st))
                , (col, True) <- zip [0 :: Int ..] (V.toList cells)
                ]
         in foldr (\(x, y) g -> setPad x y (Pad NoAction Yellow) g) g0 toggled
    SceneMode m ->
        let g0 = emptyGrid 8 8
         in foldr (\(x, y) g -> setPad x y (Pad NoAction Yellow) g) g0 (Map.keys m)
  where
    paintAll c =
        let row = replicate 8 (Pad NoAction c)
         in (emptyGrid 8 8){gridPads = replicate 8 row}

-- | Compute the pitch a pad triggers in 'InstrumentMode'.
padToPitch :: InstrumentConfig -> Int -> Int -> Pitch
padToPitch cfg x y =
    instRoot cfg + Pitch (x * instStep cfg + y * instRowStep cfg)

{- | Flip the cell at @(x, y)@ in a 'SequencerState'. Out-of-bounds coords
are a no-op (matching 'setPad' in "Funktor.Grid").
-}
setStep :: Int -> Int -> SequencerState -> SequencerState
setStep x y st
    | y < 0 || y >= V.length (seqSteps st) = st
    | x < 0 || x >= V.length (seqSteps st V.! y) = st
    | otherwise =
        let row = seqSteps st V.! y
            row' = row V.// [(x, not (row V.! x))]
            steps' = seqSteps st V.// [(y, row')]
         in st{seqSteps = steps'}
