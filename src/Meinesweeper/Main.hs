module Main where

import Meinesweeper.Game
import Graphics.UI.WX

--Easy height width and mines
easyH = 9
easyW = 9
easyM = 10

--Medium height width and mines
mediumH = 16
mediumW = 16
mediumM = 40

--Hard height width and mines
hardH = 16
hardW = 30
hardM = 99

newBoard :: String -> Int -> Int -> IO ()
newBoard title h w = do
  f <- frame [text := title]
  quit <- button f [text := "Quit"
                   ,on command := close f]
  back <- button f [text := "Main"
                   ,on click := (\b -> close f >> mainMenu)]
  set f [layout := minsize (sz 200 100) $ margin 10 $ column 1 [floatCentre $ widget quit
                                                               ,floatCentre $ widget back]]

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
