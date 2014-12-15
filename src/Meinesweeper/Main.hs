module Main where

import Meinesweeper.Game
import Graphics.UI.WX

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

newBoard :: String -> Int -> Int -> IO ()
newBoard title h w = do
  f <- frameFixed [text := title]
  let board = createBoard' h w
  boardButtons <- boardGUI' board f
  let gui = widgetise boardButtons
  quit <- button f [text := "Quit"
                   ,on command := close f]
  back <- button f [text := "Main"
                   ,on click := (\b -> close f >> mainMenu)]
  set f [layout := minsize (sz 200 100) $ margin 10 $ column 1 $ [grid h w gui] ++ [floatBottom $ widget quit, floatBottom $ widget back]]
    where
      widgetise :: [[Button ()]] -> [[Layout]]
      widgetise = map (map widget)

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

main :: IO ()
main = start mainMenu
