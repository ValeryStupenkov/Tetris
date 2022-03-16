module Tetris where


import Graphics.Gloss.Interface.Pure.Game
import System.Random
import Data.Maybe

---------------------
-- Data types
---------------------

-- Tetris figures
type Block = (Int,Int)

-- Types of figures in tetris. Names taken from tetris fandom
data FigureType = I | J | L | O | S | T | Z
  deriving (Enum, Bounded, Show)

-- Rotation of figure
data Rotation = D1 | D2 | D3 | D4
  deriving Show

-- Figures with rotation
data Figure = Figure FigureType Rotation [Block]
  deriving Show

-- Size of Grid
type GridSize = (Int, Int)

-- Position 
type GridPosition = (Int, Int)

-- Size of the window
type WindowSize = (Int, Int)

-- Position of the window
type WindowPosition = (Int, Int)

-- Size of the game in the window
type GameSize = (Float, Float)

data GameColor = Red | Yellow | Green | Violet | Blue
  deriving (Enum, Bounded, Ord, Eq)

-- Current state of the game
data AppState = AppState
  { 
    windowSize :: WindowSize
  , gameSize :: GameSize
  , gridSize :: GridSize
  , windowPosition :: WindowPosition
  , startPosition :: GridPosition
  , currentFigure :: Figure
  , currentPosition :: GridPosition
  , figureColor :: GameColor
  , nextColors :: [GameColor]
  , width :: Int
  , height :: Int
  , nextFigures :: [Figure]
  , score :: Integer
  , gameOver :: Bool
  }

-- Default state of the game
-- defaultAppState :: AppState



-- initialState :: AppState


backColor :: Color
backColor = black

fps :: Int
fps = 60

display :: Display
display = FullScreen


-----------------------
-- Functions for drawing a game
-----------------------

drawApp :: AppState -> Picture
drawApp _ = Blank

drawGrid :: AppState -> Picture
drawGrid _ = Blank

drawEmptyGrid :: AppState -> Picture
drawEmptyGrid _ = Blank

drawFigure :: Figure -> Color -> GridPosition -> Picture
drawFigure _ _ _ = Blank


-----------------------
-- Functions to move falling figures 
-----------------------

-- Move figure right if possible
moveFigureLeft :: AppState -> AppState
moveFigureLeft x = x

-- Move figure left if possible
moveFigureRight :: AppState -> AppState
moveFigureRight x = x

-- Move figure down if possible
moveFigureDown :: AppState -> AppState 
moveFigureDown x = x

-- Rotate figure
rotateFigure :: AppState -> AppState
rotateFigure x = x

-----------------------
-- Handler and update functions
-----------------------

-- Handle Events
handlerEvent :: Event -> AppState -> AppState
handlerEvent _ state = state

-- Updates game state by moving current figure down
updateApp :: Float -> AppState -> AppState
updateApp _ state = state



------------------------------
-- Main function for this app.
------------------------------

-- Run game
runGame :: IO ()
runGame = do
  let cfg = defaultAppState
  game <- cfg
  play display
       backColor 
       fps 
       game 
       (drawApp cfg)  
       handlerEvent 
       updateApp

