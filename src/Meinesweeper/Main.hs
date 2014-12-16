module Main where

import Meinesweeper.Game
import Meinesweeper.Field
import Meinesweeper.Board
import Control.Monad
import System.Random
import Graphics.UI.WX
import qualified Data.Vector as DV

--Easy height width and mines
easyH = 8
easyW = 8
easyM = 10

--Medium height width and mines
mediumH = 15
mediumW = 15
mediumM = 40

--Hard height width and mines
hardH = 15
hardW = 29
hardM = 99

newBoard :: String -> Int -> Int -> Int -> IO ()
newBoard title h w m = do
  f <- frameFixed [text := title]

  g <- newStdGen
  let board = createBoard h w m g
  boardButtons <- boardGUI board f
  let gui = widgetise h boardButtons
  quit <- button f [text := "Quit"
                   ,on command := close f]
  back <- button f [text := "Main"
                   ,on click := (\b -> close f >> mainMenu)]

  set f [layout := minsize (sz 200 100) $ margin 10 $ column 5 $ gui ++ [floatBottom $ widget quit, floatBottom $ widget back]]
    where
      widgetise :: Int -> [[Button ()]] -> [Layout]
      widgetise _ [] = []
      widgetise r (b:bs) = row r (map widget b) : widgetise r bs

mainMenu :: IO ()
mainMenu = do
  f <- frameFixed [text := "MineSweeper"]

  --Widgets
  quit <- button f [ text := "Quit"
                   , on command := close f]
  easy <- button f [ text := "Easy"
                   , on click := (\b -> close f >> newBoard "Easy" easyH easyW easyM) ]
  medium <- button f [ text := "Medium"
                     , on click := (\b -> close f >> newBoard "Medium" mediumH mediumW mediumM) ]
  hard <- button f [ text := "Hard"
                   , on click := (\b -> close f >> newBoard "Hard" hardH hardW hardM) ]

  --Frame layout
  set f [layout := minsize (sz 200 100) $ column 1
                   [hfill $ hrule 1
                   ,fill $ margin 10 $ column 1
                   [floatCentre $ widget easy
                   ,floatCentre $ widget medium
                   ,floatCentre $ widget hard
                   ,floatCentre $ widget quit]]]

fieldButton :: Frame () -> Field -> IO (Button ())
fieldButton f field
  | _covered field = makeButton f "  "
  | _flagged field = makeButton f "<|"
  | _mined field   = makeButton f "**"
  | otherwise      = makeButton f "  "
    where
      makeButton f txt = smallButton f [text := txt
                                              ,on click := (\b -> let (x,y) = getPoint b
                                                                  in leftClickField x y)
                                              ,on clickRight := (\b -> let (x,y) = getPoint b
                                                                       in rightClickField x y)]
      getPoint p = (pointX p, pointY p)

boardGUI :: Board -> Frame () -> IO [[Button ()]]
boardGUI b f = mapM (mapM (fieldButton f)) (DV.toList $ DV.map DV.toList b)

main :: IO ()
main = start mainMenu
