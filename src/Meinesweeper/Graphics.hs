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

newBoard :: Int -> Int -> Int -> IO ()
newBoard h w m = do
  g <- newStdGen
  let gameState = newMeinesweeper h w m g
  makeGUI gameState h

mainMenu :: IO ()
mainMenu = do

  f <- frameFixed [text := "MineSweeper"]

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

fieldButton :: Int -> Frame () -> Meinesweeper -> Field -> IO (Button ())
fieldButton h f gameState field
  | _flagged field = makeButton h f gameState xy "F"
  | _covered field = makeButton h f gameState xy " "
  | _mined field   = makeButton h f gameState xy "*"
  | otherwise      = makeButton h f gameState xy (show $ _adjacentMines field)
  where
   xy = _xy field
   makeButton h f game (x,y) txt = smallButton f [text := txt
                                                 ,on command := let (win,state) = runState (leftClickField x y) game
                                                                in close f >> makeGUI state h]--putStrLn (show x ++ " " ++ show y) ]{-->> makeGUI g f h]--}

boardGUI :: Board -> Int -> Frame () -> Meinesweeper -> IO [[Button ()]]
boardGUI b h f g = mapM (mapM (fieldButton h f g)) (DV.toList $ DV.map DV.toList b)

widgetise :: Int -> [[Button ()]] -> [Layout]
widgetise _ [] = []
widgetise r (b:bs) = row r (map widget b) : widgetise r bs

makeGUI :: Meinesweeper -> Int -> IO ()
makeGUI gameState h = do
  --game <- varGet gameState
  f <- frameFixed [text := "Meinesweeper"]
  let board = _board gameState
  boardButtons <- boardGUI board h f gameState
  let gui = widgetise h boardButtons

  quit <- button f [text := "Quit"
                   ,on command := close f]
  back <- button f [text := "Main"
                   ,on command := (close f >> mainMenu)]

  set f [layout := minsize (sz 200 100) $ margin 10 $ column 0 $ gui ++ [floatBottom $ widget back, floatBottom $ widget quit]
        ,on click := \p -> let x = pointX p
                               y = pointY p
                           in putStrLn (show x ++ " " ++ show y)]
