module Moving where


import System.Random

import State
import Drawing
-----------------------
-- Functions to move falling figures 
-----------------------

-- Move figure right or left if possible
moveFigure :: AppState -> Int -> AppState
moveFigure state x
  | canPlaceFigure (figure state) pos (well state) = state {figurePos = pos}
  | otherwise = state
    where pos = (fst (figurePos state) + x, snd (figurePos state))


-- Fuction to organize rotating of the falling figure
rotateFigure :: AppState -> AppState
rotateFigure state 
  | canPlaceFigure (rotatedFigure (figure state)) (figurePos state) (well state) = state {figure = rotatedFigure (figure state)}
  | otherwise = state

-- Just changes coordinates of the figure
-- It will rotate her
rotatedFigure :: Figure -> Figure   
rotatedFigure (Figure cs col) = Figure (map rot cs) col
  where rot (x,y) = (y, -x)

-- Check if it is possible to place figure at the coords
canPlaceFigure :: Figure -> (Int, Int) -> Well -> Bool
canPlaceFigure fig coord w = isInField && (not collision)
  where
    isInField = okPos coord fig
    collision = figureCollision fig coord w
   
-- Check if position is valid 
okPos :: (Int, Int) -> Figure -> Bool
okPos (x, y) (Figure cs _) = and (map okCoord cs)
  where okCoord (px, py) = (px + x >= -9) && (px + x <= 9) && (py +y <= 1) && (py+y >= -41) 

-- Check if figure collides with other figures
figureCollision :: Figure -> (Int, Int) -> Well -> Bool 
figureCollision fig figpos w = wellCollide draw w
  where 
    draw = drawFigure fig figpos emptyWell
    wellCollide (Well w1) (Well w2) = or (map rowCollide (zip w1 w2))
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
  | (snd (figurePos state)) > (-2) = state {gameOver = True, isPause = True, maxScore = (score state)}
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