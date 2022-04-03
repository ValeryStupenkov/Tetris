module RunTetris where

import Graphics.Gloss.Interface.Pure.Game

import State
import Drawing
import Moving


-----------------------
-- Handler function for play
-----------------------

-- Handle Events
handlerEvent :: Event -> AppState -> AppState
handlerEvent (EventKey (Char 'p') Down _ _) state = state {isPause = not $ isPause state}

handlerEvent (EventKey (SpecialKey KeyLeft) Down _ _) state = case isPause state of 
                                                                False -> moveFigure state (-2)
                                                                True -> state

handlerEvent (EventKey (SpecialKey KeyRight) Down _ _) state = case isPause state of 
                                                                False -> moveFigure state 2
                                                                True -> state

handlerEvent (EventKey (SpecialKey KeyUp) Down _ _) state = case isPause state of 
                                                                False -> rotateFigure state 
                                                                True -> state

handlerEvent (EventKey (SpecialKey KeyDown) Down _ _) state = state {pressedDown = True}

handlerEvent (EventKey (SpecialKey KeyDown) Up _ _) state = state {pressedDown = False}

handlerEvent (EventKey (SpecialKey KeySpace) Down _ _) state = resetState state

handlerEvent _ state = state 

-----------------------
-- Update function for play
-----------------------

-- Updates game state by moving current figure down
-- This fuction calls for updateGame and changes time
updateApp :: Float -> AppState -> AppState
-- updateApp t state = updateGame (state {time = (time state + t), deltaTime = t})
updateApp t state 
  | isPause state = state
  | otherwise = updateGame (state { deltaTime = t})

-- I decided that it's easier to use some helping function
-- Moves figure down over time
updateGame :: AppState -> AppState
updateGame state 
  | timeToMove newstate <= 0 = moveFigureDown newstate {timeToMove = updateSpeed state}
  | otherwise = newstate 
    where 
      newstate = state {timeToMove = (timeToMove state) - (deltaTime state)}

-- This one changes speed of falling figure based on pressedDown value 
updateSpeed :: AppState -> Float
updateSpeed state
  | pressedDown state = 1.0 / 20
  | otherwise = 1.0 / 2


------------------------------
-- Main function for this app
------------------------------

-- Some constatnts for play fuction
backColor :: Color
backColor = black

fps :: Int
fps = 60

display :: Display
display = InWindow "Tetris" (screenWidth, screenHeight) (200, 200)
  where
    screenWidth = 1280
    screenHeight = 768

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
