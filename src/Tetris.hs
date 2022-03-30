module Tetris where


import Graphics.Gloss.Interface.Pure.Game
import System.Random
import Data.Maybe

---------------------
-- Data types
---------------------

-- Type for tetris figures
data Figure = Figure [(Int,Int)] Color 
  deriving (Show)

-- Figures 
figureI = Figure [(-3,-1), (-1,-1), (1,-1), (3,-1)] cyan
figureJ = Figure [(-1, -1), (1, -1), (3,-1), (-1, 1)] blue
figureL = Figure [(-3,-1),(-1, -1), (1, -1), (1, 1)] orange
figureO = Figure [(-1, -1), (1, -1), (-1, 1), (1, 1)] yellow
figureS = Figure [(-1, -1), (1, -1), (1, 1), (3,1)] green
figureT = Figure [(-1, -1), (1, -1), (3,-1), (1, 1)] red
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
  , time :: Float
  , deltaTime :: Float
  , timeToMove :: Float
  , figure :: Figure
  , figurePos :: FigurePos
  , score :: Int
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
  , time = 0
  , deltaTime = 0
  , timeToMove = 0
  , figure = figureO
  , figurePos = (0, 0)
  , score = 0
  , pressedDown = False
  , isPause = False
  , gameOver = False
  , randomGen = mkStdGen 0
  }

-- Reset state to default and generate new random value
resetState :: AppState -> AppState
resetState state = defaultAppState {randomGen = randomGen state}

-----------------------
-- Functions for drawing a game
-----------------------

-- These are constants which will be used in draw functions
-- Size of one cell in pixels
cellSize :: Int
cellSize = 35

-- Color of the wall
wallColor :: Color
wallColor = dark (dark blue) 

-- Color of the well
wellColor :: Color
wellColor = black

-- Width of the well
wellWidth :: Int
wellWidth = 10 * cellSize

-- Height of the well
wellHeight :: Int
wellHeight = 20 * cellSize

-- Thickness of the borders
border :: Int
border = (768 - (20 * cellSize)) `quot` 2 

-- Width of the wall
wallWidth :: Int
wallWidth = wellWidth + 2 * border

-- Height of the wall
wallHeight :: Int
wallHeight = wellHeight + 2 * border

-- These are fucntions that are used in draw functions
-- Returns color of the cell
cellColor :: Cell -> Color
cellColor (FilledWith col) = col
cellColor _ = black

-- Concantenate each row with its number
numberOfRows :: Well -> [(Int, Row)]
numberOfRows (Well x) = zip [1,-1..(-41)] x

-- Concantenate each cell in a row with its number
numberOfCells :: Row -> [(Int, Cell)]
numberOfCells (Row x) = zip [-9,-7..9] x 

-- Returns color of the figure
figureColor :: Figure -> Color
figureColor (Figure _ col) = col

-- Check if figure is on this coordinates
figureContain :: (Int, Int) -> Figure -> Bool
figureContain c (Figure x _) = elem c x

-- Finally, draw functions themselves
-- Drawing fuction for play 
drawApp :: AppState -> Picture
drawApp state = pictures [walls, field, currentFigure, drawScore ]
  where 
    walls = color wallColor (rectangleSolid (fromIntegral wallWidth) (fromIntegral wallHeight))
    field = pictures [ color wellColor (rectangleSolid (fromIntegral wellWidth) (fromIntegral wellHeight)), drawWell (well state)]
    currentFigure = drawWell (drawFigure (figure state) (figurePos state) emptyWell)
    drawScore = translate (-600.0) (200.0) (scale 0.4 0.4 (pictures [playerScore]))
      where 
        playerScore = color white (Text scoretext)
        scoretext = "Score: " ++ (show (score state))

-- Function to draw a well, which consists of cells 
drawWell :: Well -> Picture
drawWell well = pictures (map pictureCell (cellPos well))
  where 
    pictureCell (x,y,c) | y > (-3) = pictures []
                        | c == Empty = pictures []
                        | otherwise = drawCell (x, y) (cellColor c)

-- Returns list of coordinates of the cells
cellPos :: Well -> [(Int, Int, Cell)]
cellPos well = concat (map takeCells (numberOfRows well))
  where 
    takeCells (y, cs) = map extractCell (numberOfCells cs)
      where 
        extractCell (x, c) = (x, y, c)

-- Draws a single cell
drawCell :: (Int, Int) -> Color -> Picture
drawCell (x,y) c = translate (fromIntegral sx) (fromIntegral sy) (color c (rectangleSolid sz sz))
  where
    sx = fst transformed
    sy = snd transformed
    sz = 0.9 * (fromIntegral cellSize)
    transformed = transformCoords (x, y)

-- Transform coordinates from playfield to screen coordinates
transformCoords :: (Int,Int) -> (Int, Int)
transformCoords (x,y) = (sx,sy) 
  where
    sx = quot (x * cellSize) 2
    sy = (11 * cellSize) + quot (y * cellSize) 2

-- It's not drawing exactly, fuction just changes color of the cells in the well if a figure is in position
-- drawWell later will draw all cells in the right color 
drawFigure :: Figure -> (Int,Int) -> Well -> Well
drawFigure figure (x, y) well 
  | (odd x) || (odd y) = error "Incorrect: piece coordinates must be even"  
  | otherwise = Well (map drawRow (numberOfRows well))
    where
      drawRow (py, row) = Row (map drawcell (numberOfCells row))
        where drawcell (px, c) 
                             | c /= Empty = c
                             | figureContain (px - x, py - y) figure = FilledWith (figureColor figure)
                             | otherwise = Empty


-----------------------
-- Handler function for play
-----------------------

-- Handle Events
handlerEvent :: Event -> AppState -> AppState
handlerEvent (EventKey (Char 'p') Down _ _) state = state {isPause = not $ isPause state}

handlerEvent (EventKey (SpecialKey KeyLeft) Down _ _) state = case isPause state of 
                                                                False -> moveFigureLeft state
                                                                True -> state

handlerEvent (EventKey (SpecialKey KeyRight) Down _ _) state = case isPause state of 
                                                                False -> moveFigureRight state 
                                                                True -> state

handlerEvent (EventKey (SpecialKey KeyUp) Down _ _) state = case isPause state of 
                                                                False -> rotateFigure state 
                                                                True -> state

handlerEvent (EventKey (SpecialKey KeyDown) Down _ _) state = state {pressedDown = True}

handlerEvent (EventKey (SpecialKey KeyDown) Up _ _) state = state {pressedDown = False}

handlerEvent _ state = state 

-----------------------
-- Functions to move falling figures 
-----------------------

-- Move figure right if possible
moveFigureLeft :: AppState -> AppState
moveFigureLeft state 
  | canPlaceFigure (figure state) pos (well state) = state {figurePos = pos}
  | otherwise = state
    where pos = (fst (figurePos state) - 2, snd (figurePos state))

-- Move figure left if possible
moveFigureRight :: AppState -> AppState
moveFigureRight state 
  | canPlaceFigure (figure state) pos (well state) = state {figurePos = pos}
  | otherwise = state
    where pos = (fst (figurePos state) + 2, snd (figurePos state))

-- Fuction to organize rotating of the falling figure
rotateFigure :: AppState -> AppState
rotateFigure state 
  | canPlaceFigure (rotatedFigure (figure state)) (figurePos state) (well state) = state {figure = rotatedFigure (figure state)}
  | otherwise = state

-- Just changes coordinates of the figure
-- It will rotate her
rotatedFigure :: Figure -> Figure   
rotatedFigure (Figure cs col) = Figure (map rotate cs) col
  where rotate (x,y) = (y, -x)

-- Check if it is possible to place figure at the coords
canPlaceFigure :: Figure -> (Int, Int) -> Well -> Bool
canPlaceFigure figure coord well = isInField && (not collision)
  where
    isInField = okPos coord figure
    collision = figureCollision figure coord well
   
-- Check if position is valid 
okPos :: (Int, Int) -> Figure -> Bool
okPos (x, y) (Figure cs _) = and (map okCoord cs)
  where okCoord (px, py) = (px + x >= -9) && (px + x <= 9) && (py +y <= 1) && (py+y >= -41) 

-- Check if figure collides with other figures
figureCollision :: Figure -> (Int, Int) -> Well -> Bool 
figureCollision figure figurePos well = wellsCollide draw well
  where 
    draw = drawFigure figure figurePos emptyWell
    wellsCollide (Well w1) (Well w2) = or (map rowCollide (zip w1 w2))
    rowCollide ((Row r1), (Row r2)) = or (map cellCollide (zip r1 r2))
    cellCollide (a,b) = (a /= Empty) && (b /= Empty)

-- This fuctions is for update
moveFigureDown :: AppState -> AppState
moveFigureDown state
  | not (canPlaceFigure (figure state) newpos (well state)) = handleFullRow (changeFig state)
  | otherwise = state {figurePos = newpos}
    where 
      newpos = (fst (figurePos state), snd (figurePos state) -2)

-- Fix falled figure and change it to the new one
changeFig :: AppState -> AppState
changeFig state 
  | (snd (figurePos state)) > (-2) = resetState state
  | otherwise = state {
                        well = drawFigure (figure state) (figurePos state) (well state)
                      , figure = randomFigure (fst (reGen state))
                      , figurePos = (0, 0)
                      , randomGen = snd (reGen state)
                      , pressedDown = False
                      } 
                      
-- Generate random value and returns it
reGen :: AppState -> (Int, StdGen)
reGen state = randomR (0, 6) (randomGen state)

-- Function to handle a full rows
-- It changes score and calls for another fuction that clears the full rows and count them
handleFullRow :: AppState -> AppState
handleFullRow state = state {well = fst result, score = (score state) + (snd result)}
  where 
    result = countFilledRows (well state)

-- Counts and clear all full rows
countFilledRows :: Well -> (Well, Int)
countFilledRows (Well x) = (newWell, count)
  where
    newWell = Well (clear ++ rest)
    rest = filter notFull x
    count :: Int
    count = (length x) - (length rest)
    clear :: [Row]
    clear = replicate count emptyRow 
    notFull (Row y) = not (and (map (\c -> c /= Empty) y))    



-----------------------
-- Update function for play
-----------------------

-- Updates game state by moving current figure down
-- This fuction calls for updateGame and changes time
updateApp :: Float -> AppState -> AppState
updateApp t state = updateGame (state {time = (time state + t), deltaTime = t})

-- I decided that it's easier to use some helping function
-- Moves figure down over time
updateGame :: AppState -> AppState
updateGame state 
  | timeToMove newstate <= 0 = moveFigureDown newstate {timeToMove = updateSpeed state}
  | otherwise = newstate 
    where 
      newstate = state {timeToMove = (timeToMove state) - (deltaTime state)}


updateSpeed :: AppState -> Float
updateSpeed state
  | pressedDown state = 1.0 / 20
  | otherwise = 1.0 / 2

------------------------------
-- Main function for this app.
------------------------------

-- Some constatnts for play fuction
backColor :: Color
backColor = black

fps :: Int
fps = 30

screenWidth :: Int
screenWidth = 1280

screenHeight :: Int
screenHeight = 768

display :: Display
display = InWindow "Tetris" (screenWidth, screenHeight) (200, 200)

-- Run game
runGame :: IO ()
runGame = do
  let cfg = defaultAppState
  play display
       backColor 
       fps 
       cfg
       drawApp
       handlerEvent 
       updateApp

