{- | Console UI for Funktor.

A small text-mode dashboard: tempo, transport state, current beat, and the
8×8 grid rendered as ASCII squares. The pure reducer ('applyEvent') and
renderer ('renderUI') are exported separately from the IO loop ('runUI')
so they can be tested without a terminal.

Switching to a full @brick@ TUI is an isolated change to 'runUI'; the
'UIState' shape and reducer stay the same.
-}
module Funktor.UI (
    UIState (..),
    CustomEvent (..),
    initialUIState,
    applyEvent,
    renderUI,
    runUI,
) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (TVar, readTVarIO)
import Control.Monad (forever)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Maybe (isJust)
import Data.Vector qualified as V

import Funktor.Audio.Scheduler (SchedulerState (..))
import Funktor.Audio.State (AudioState (..))
import Funktor.Audio.Voice (VoicePool (..))
import Funktor.Core.Types (Beat (..), Duration (..), Tempo (..))
import Funktor.Grid (Color (..), Grid (..), Pad (..), PadAction (..), emptyGrid)

-- | Custom events pushed into the TUI from background threads.
data CustomEvent
    = Tick
    | SetTempo !Tempo
    | SetBeat !Beat
    | SetPlaying !Bool
    | MoveCursor !Int !Int
    deriving (Show)

-- | The terminal UI state.
data UIState = UIState
    { uiGrid :: !Grid
    , uiCursorRow :: !Int
    , uiCursorCol :: !Int
    , uiTempo :: !Tempo
    , uiCurrentBeat :: !Beat
    , uiPatternLen :: !Duration
    , uiPlaying :: !Bool
    }
    deriving (Eq, Show)

initialUIState :: UIState
initialUIState =
    UIState
        { uiGrid = emptyGrid 8 8
        , uiCursorRow = 0
        , uiCursorCol = 0
        , uiTempo = Tempo 120
        , uiCurrentBeat = Beat 0
        , uiPatternLen = Duration 8
        , uiPlaying = False
        }

{- | Pure event reducer. 'Tick' is a no-op; the wall-clock state arrives via
the directed setters so the reducer remains independent of @IO@.
-}
applyEvent :: CustomEvent -> UIState -> UIState
applyEvent Tick s = s
applyEvent (SetTempo t) s = s{uiTempo = t}
applyEvent (SetBeat b) s = s{uiCurrentBeat = b}
applyEvent (SetPlaying p) s = s{uiPlaying = p}
applyEvent (MoveCursor r c) s = s{uiCursorRow = clampCursor r, uiCursorCol = clampCursor c}
  where
    clampCursor x = max 0 (min 7 x)

{- | Render the state as a list of lines. Each grid row is 8 colored squares;
the cursor is highlighted with @[]@ instead of a square.
-}
renderUI :: UIState -> [String]
renderUI s =
    [ "Funktor — " ++ transport
    , "Tempo: " ++ tempoStr ++ " bpm  Beat: " ++ beatStr ++ "/" ++ durStr
    , ""
    ]
        ++ gridLines
  where
    transport = if s.uiPlaying then "Playing" else "Stopped"
    tempoStr = show (round (unTempo s.uiTempo) :: Int)
    beatStr = show (fromRational (unBeat s.uiCurrentBeat) :: Double)
    durStr = show (fromRational (unDuration s.uiPatternLen) :: Double)
    gridLines =
        let padRows = s.uiGrid.pads
         in [ concat [cell r c (padAt padRows r c) | c <- [0 .. s.uiGrid.width - 1]]
            | r <- [s.uiGrid.height - 1, s.uiGrid.height - 2 .. 0]
            ]
    padAt rows r c
        | r < length rows
        , let row = rows !! r
        , c < length row =
            row !! c
        | otherwise = Pad NoAction Off
    cell r c pad =
        let isCursor = r == s.uiCursorRow && c == s.uiCursorCol
            sym = colorChar pad.color
         in if isCursor then "[" ++ sym ++ "]" else " " ++ sym ++ " "

colorChar :: Color -> String
colorChar Off = "."
colorChar Red = "R"
colorChar Green = "G"
colorChar Yellow = "Y"
colorChar Blue = "B"
colorChar Purple = "P"
colorChar Cyan = "C"
colorChar White = "W"

{- | Run the dashboard, polling the scheduler + audio engine at ~10 Hz and
redrawing on each tick. Uses ANSI cursor-home to overdraw the previous
frame; on a terminal without ANSI the output appends instead. Press
Ctrl-C to exit.
-}
runUI :: TVar AudioState -> TVar SchedulerState -> IO ()
runUI audioVar schedVar = do
    stateRef <- newIORef initialUIState
    forever $ do
        sched <- readTVarIO schedVar
        audio <- readTVarIO audioVar
        let liveCount = V.length (V.filter isJust audio.pool.voices)
        prev <- readIORef stateRef
        let updated =
                prev
                    & applyEvent (SetTempo sched.tempo)
                    & applyEvent (SetBeat sched.beat)
                    & applyEvent (SetPlaying True)
        writeIORef stateRef updated
        putStr "\ESC[H\ESC[2J" -- clear screen + home cursor (ANSI)
        mapM_ putStrLn (renderUI updated)
        putStrLn ("Active voices: " ++ show liveCount)
        threadDelay 100000

(&) :: a -> (a -> b) -> b
x & f = f x
infixl 1 &
