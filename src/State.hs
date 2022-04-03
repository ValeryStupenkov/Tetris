module State where

import Graphics.Gloss.Interface.Pure.Game
import System.Random

---------------------
-- Data types
---------------------


-- Type for tetris figures
data Figure = Figure [(Int,Int)] Color 
  deriving (Show)

-- Figures 
figureI :: Figure
figureI = Figure [(-3,-1), (-1,-1), (1,-1), (3,-1)] cyan
figureJ :: Figure 
figureJ = Figure [(-1, -1), (1, -1), (3,-1), (-1, 1)] blue
figureL :: Figure
figureL = Figure [(-3,-1),(-1, -1), (1, -1), (1, 1)] orange
figureO :: Figure
figureO = Figure [(-1, -1), (1, -1), (-1, 1), (1, 1)] yellow
figureS :: Figure
figureS = Figure [(-1, -1), (1, -1), (1, 1), (3,1)] green
figureT :: Figure
figureT = Figure [(-1, -1), (1, -1), (3,-1), (1, 1)] red
figureZ :: Figure
figureZ = Figure [(-1, -1), (1, -1), (-3,1), (-1, 1)] magenta

-- Chooses one of the figures by their number
randomFigure :: Int -> Figure
randomFigure x = case x of
  0 -> figureI
  1 -> figureJ
  2 -> figureL
  3 -> figureO
  4 -> figureS
  5 -> figureT
  6 -> figureZ
  _ -> figureO

-- Position of the figure
type FigurePos = (Int, Int)

-- Rectangle in the field
data Cell = Empty | FilledWith Color 
  deriving (Show, Eq)

-- Row of cells
data Row = Row [Cell]
  deriving (Show)

-- Well
-- It is basically the play field where the figures are falling
-- Well consists of rows
data Well = Well [Row] 
  deriving (Show)

-- Constants for creating an empty well and empty row
emptyRow :: Row
emptyRow = Row (replicate 10 Empty)

emptyWell :: Well
emptyWell = Well (replicate 22 emptyRow)

-- Current state of the game
data AppState = AppState
  { 
    well :: Well
  , deltaTime :: Float
  , timeToMove :: Float
  , figure :: Figure
  , figurePos :: FigurePos
  , score :: Int
  , maxScore :: Int
  , pressedDown :: Bool
  , isPause :: Bool
  , gameOver :: Bool
  , randomGen :: StdGen 
  }

-- Default state of the game
defaultAppState :: AppState
defaultAppState = AppState
  {
    well = emptyWell
  , deltaTime = 0
  , timeToMove = 0
  , figure = figureO
  , figurePos = (0, 0)
  , score = 0
  , maxScore = 0
  , pressedDown = False
  , isPause = False
  , gameOver = False
  , randomGen = mkStdGen 0
  }

-- Reset state to default and generate new random value
resetState :: AppState -> AppState
resetState state = defaultAppState {randomGen = randomGen state, maxScore = maxScore state}