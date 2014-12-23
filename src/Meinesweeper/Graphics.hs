module Meinesweeper.Graphics (startGraphicsLoop) where

import Meinesweeper.Game
import Meinesweeper.Field
import Meinesweeper.Board
import Meinesweeper.Constants
import Control.Monad.State
import System.Random
import Graphics.UI.WX
import qualified Data.Vector as DV

startGraphicsLoop :: IO () -- entry point for export
startGraphicsLoop = start mainMenu

-- Create a new random gen and create Meinesweeper
-- Make the game window using the Meinesweeper
newBoard :: Int -> Int -> Int -> IO ()
newBoard h w m = do
    g <- newStdGen
    let gameState = newMeinesweeper h w m g
    makeGUI gameState h w m

-- Difficulty menu
mainMenu :: IO ()
mainMenu = do

    f <- frameFixed [text := "MeineSweeper"]

    --Widgets
    quit <- button f [ text := "Quit"
                     , on command := close f]
    easy <- button f [ text := "Easy"
                     , on command := (close f >> newBoard easyH easyW easyM) ]
    medium <- button f [ text := "Medium"
                       , on command := (close f >> newBoard mediumH mediumW mediumM) ]
    hard <- button f [ text := "Hard"
                     , on command := (close f >> newBoard hardH hardW hardM) ]

    --Frame layout
    set f [layout := minsize (sz 200 100) $ column 1
                     [hfill $ hrule 1
                     ,fill $ margin 10 $ column 1
                     [floatCentre $ widget easy
                     ,floatCentre $ widget medium
                     ,floatCentre $ widget hard
                     ,floatCentre $ widget quit]]]

-- Create a button from a field
-- Set event for a left click
--    if mine is clicked then the player is shown the loser screen
--    otherwise if the game is won the player is shown the winner screen
--              otherwise refreshes screen with button change
-- Set even for right click
--    update frame with button change
fieldButton :: Frame () -> Int -> Int -> Int -> Meinesweeper -> Field -> IO (Button ())
fieldButton f h w m gameState field
    | _flagged field = makeButton f h w m gameState xy "F"
    | _covered field = makeButton f h w m gameState xy "  "
    | _mined field   = makeButton f h w m gameState xy "_"
    | otherwise      = makeButton f h w m gameState xy (show $ _adjacentMines field)
    where
     xy = _xy field
     makeButton f h w m game (x,y) txt = smallButton f [text := txt
                                                   ,on click := \p -> let (win,s) = runState (leftClickField (x - 1) (y - 1)) game
                                                                      in if win then let (won,s') = runState isWon s
                                                                                     in if won then close f >> winloseScreen "You WON!"
                                                                                               else close f >> makeGUI s' h w m
                                                                                else close f >> makeGUI s h w m >> winloseScreen "You LOST!"
                                                   ,on clickRight := \p -> let (_,s) = runState (rightClickField (x - 1) (y - 1)) game
                                                                           in close f >> makeGUI s h w m]
-- Screen to show if the player won or lost
winloseScreen :: String -> IO ()
winloseScreen txt = do
    f <- frameFixed [text := txt
                    ,on paint := paintMessageText txt]

    m <- button f [text := "Main Menu"
                  ,on command := close f >> mainMenu]

    set f [layout := minsize (sz 200 100) $ margin 5 $ column 5 [floatBottom $ widget m]]

paintMessageText :: String -> DC a -> t -> IO ()
paintMessageText txt dc area = drawText dc txt (pt 10 10) [fontSize := 36]

-- Create a board of buttons using fieldButton function
boardGUI :: Board -> Frame () -> Int -> Int -> Int -> Meinesweeper -> IO [[Button ()]]
boardGUI b f h w m g = mapM (mapM (fieldButton f h w m g)) (DV.toList $ DV.map DV.toList b)

-- Turn the buttons into widgets
widgetise :: Int -> [[Button ()]] -> [Layout]
widgetise _ [] = []
widgetise r (b:bs) = row r (map widget b) : widgetise r bs

-- Create the GUI board and display it in a window
makeGUI :: Meinesweeper -> Int -> Int -> Int -> IO ()
makeGUI gameState h w m = do
    f <- frameFixed [text := "Meinesweeper"]

    let board = _board gameState
    boardButtons <- boardGUI board f h w m gameState
    let gui = widgetise h boardButtons

    solver <- button f [text := "Solve"]

    quit <- button f [text := "Quit"
                     ,on command := close f]
    back <- button f [text := "New Game"
                     ,on command := close f >> mainMenu]

    set f [layout := minsize (sz 200 100) $ margin 5 $ column 5 $ [floatTop $ widget solver] ++ gui ++ [margin 5 $ column 5 [floatBottom $ widget back, floatBottom $ widget quit]]]
