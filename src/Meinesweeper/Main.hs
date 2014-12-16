module Main where

import Meinesweeper.Game
import Graphics.UI.WX
import Meinesweeper.Field
import Meinesweeper.Board
import qualified Data.Vector as DV
import Control.Monad

--Easy height width and mines
easyH :: Int
easyH = 8
easyW :: Int
easyW = 8
easyM :: Int
easyM = 10

--Medium height width and mines
mediumH :: Int
mediumH = 15
mediumW :: Int
mediumW = 15
mediumM :: Int
mediumM = 40

--Hard height width and mines
hardH :: Int
hardH = 15
hardW :: Int
hardW = 29
hardM :: Int
hardM = 99

newBoard :: String -> Int -> Int -> IO ()
newBoard title h w = do
  f <- frameFixed [text := title]

  let board = createBoard h w
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
                   , on click := (\b -> close f >> newBoard "Easy" easyH easyW) ]
  medium <- button f [ text := "Medium"
                     , on click := (\b -> close f >> newBoard "Medium" mediumH mediumW) ]
  hard <- button f [ text := "Hard"
                   , on click := (\b -> close f >> newBoard "Hard" hardH hardW) ]

  --Frame layout
  set f [layout := minsize (sz 200 100) $ column 1
                   [hfill $ hrule 1
                   ,fill $ margin 10 $ column 1
                   [floatCentre $ widget easy
                   ,floatCentre $ widget medium
                   ,floatCentre $ widget hard
                   ,floatCentre $ widget quit]]]

fieldButton :: Frame () -> Field -> IO (Button ())
fieldButton panel field
  | _flagged field = makeButton panel "<|"
  | _covered field = makeButton panel "  "
  | _mined field   = makeButton panel "**"
  | otherwise      = makeButton panel "  "
  where
   makeButton p txt = smallButton p [text := txt]

boardGUI :: Board -> Frame () -> IO [[Button ()]]
boardGUI b f = mapM (mapM (fieldButton f)) (DV.toList $ DV.map DV.toList b)

main :: IO ()
main = start mainMenu
