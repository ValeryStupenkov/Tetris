module Drawing where

import Graphics.Gloss.Interface.Pure.Game

import State

-----------------------
-- Functions for drawing a game
-----------------------

-- These are fucntions that are used in draw functions

-- Size of one cell in pixels
-- Yes it is constant but it really better to declare her before the following fucntions
cellSize :: Int
cellSize = 35

-- Returns color of the cell
cellColor :: Cell -> Color
cellColor (FilledWith col) = col
cellColor _ = black

-- Concantenate each row with its number
numberedRows :: Well -> [(Int, Row)]
numberedRows (Well x) = zip [1,-1..(-41)] x

-- Concantenate each cell in a row with its number
numberedCells :: Row -> [(Int, Cell)]
numberedCells (Row x) = zip [-9,-7..9] x 

-- Returns color of the figure
figureColor :: Figure -> Color
figureColor (Figure _ col) = col

-- Check if figure is on this coordinates
figureContain :: (Int, Int) -> Figure -> Bool
figureContain c (Figure x _) = elem c x

-- Finally, draw functions themselves
-- Drawing fuction for play 
drawApp :: AppState -> Picture
drawApp state = pictures [walls, field, currentFigure, drawScore, gameover, maxscore, pressSpace]
  where 
    -- Draws walls
    walls = color wallColor (rectangleSolid (fromIntegral wallWidth) (fromIntegral wallHeight))
      where  
        wallColor = dark (dark blue)

    -- Draws well
    field = pictures [ color wellColor (rectangleSolid (fromIntegral wellWidth) (fromIntegral wellHeight)), drawWell (well state)]
      where
        wellColor = black

    -- Draws falling figure in the well
    currentFigure = drawWell (drawFigure (figure state) (figurePos state) emptyWell)

    -- Draws score 
    drawScore = translate (-600.0) (100.0) (scale 0.4 0.4 (pictures [playerScore]))
      where 
        playerScore = color white (Text scoretext)
        scoretext = "Score: " ++ (show (score state))

    -- Draws game over
    gameover = if (gameOver state) then translate (-600.0) (200.0) (scale 0.4 0.4 (pictures [drawFinal])) else Blank
      where 
        drawFinal = color red (Text finaltext)
        finaltext = "Game Over"
        
    -- Draws "press space to restart"
    pressSpace = if (gameOver state) then translate (-600.0) (-50.0) (scale 0.2 0.2 (pictures [space])) else Blank
      where 
        space = color white (Text spacetext)
        spacetext = "Press SPACE to restart"
        
    -- Draws max score
    maxscore = translate (-600.0) (50.0) (scale 0.4 0.4 (pictures [maxtext]))
      where
        maxtext = color blue (Text mtext)
        mtext = "Max score: " ++ (show (maxScore state))
    -- Helping constants for upper fuctions
    -- Width of the well
    wellWidth = 10 * cellSize
    -- Height of the well
    wellHeight = 20 * cellSize
    -- Thickness of the borders
    border = (768 - (20 * cellSize)) `quot` 2 
    -- Width of the wall
    wallWidth = wellWidth + 2 * border
    -- Height of the wall
    wallHeight = wellHeight + 2 * border


-- Function to draw a well, which consists of cells 
drawWell :: Well -> Picture
drawWell w = pictures (map pictureCell (cellPos w))
  where 
    pictureCell (x,y,c) | y > (-3) = pictures []
                        | c == Empty = pictures []
                        | otherwise = drawCell (x, y) (cellColor c)

-- Returns list of coordinates of the cells
cellPos :: Well -> [(Int, Int, Cell)]
cellPos w = concat (map takeCells (numberedRows w))
  where 
    takeCells (y, cs) = map extractCell (numberedCells cs)
      where 
        extractCell (x, c) = (x, y, c)

-- Draws a single cell
drawCell :: (Int, Int) -> Color -> Picture
drawCell (x,y) c = translate (fromIntegral tx) (fromIntegral ty) (color c (rectangleSolid sz sz))
  where
    tx = fst transformed
    ty = snd transformed
    sz = 0.9 * (fromIntegral cellSize)
    transformed = transformCoords (x, y)

-- Transform coordinates from playfield to screen coordinates
transformCoords :: (Int,Int) -> (Int, Int)
transformCoords (x,y) = (tx,ty) 
  where
    tx = quot (x * cellSize) 2
    ty = (11 * cellSize) + quot (y * cellSize) 2

-- It's not drawing exactly, fuction just changes color of the cells in the well if a figure is in position
-- drawWell later will draw all cells in the right color 
drawFigure :: Figure -> (Int,Int) -> Well -> Well
drawFigure fig (x, y) w
  | (odd x) || (odd y) = error "Incorrect: piece coordinates must be even"  
  | otherwise = Well (map drawRow (numberedRows w))
    where
      drawRow (py, row) = Row (map drawcell (numberedCells row))
        where drawcell (px, c) 
                             | c /= Empty = c
                             | figureContain (px - x, py - y) fig = FilledWith (figureColor fig)
                             | otherwise = Empty
