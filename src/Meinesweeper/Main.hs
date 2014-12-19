module Main where

import Meinesweeper.Game
import Meinesweeper.Field
import Meinesweeper.Board
import System.Random
import Graphics.UI.WX
import qualified Data.Vector as DV
import Control.Monad.State

--Easy height width and mines
easyH = 8 :: Int
easyW = 8 :: Int
easyM = 10 :: Int

--Medium height width and mines
mediumH = 15 :: Int
mediumW = 15 :: Int
mediumM = 40 :: Int

--Hard height width and mines
hardH = 15 :: Int
hardW = 29 :: Int
hardM = 99 :: Int

newBoard :: String -> Int -> Int -> Int -> IO ()
newBoard title h w m = do
  f <- frameFixed [text := title]

  g <- newStdGen
  gameState <- varCreate $ newMeinesweeper h w m g

  game <- varGet gameState

  --display board
  let board = _board game
  boardButtons <- boardGUI board f gameState
  let gui = widgetise h boardButtons

  quit <- button f [text := "Quit"
                   ,on command := close f]
  back <- button f [text := "Main"
                   ,on command := (close f >> mainMenu)]

  set f [layout := minsize (sz 200 100) $ margin 10 $ column 0 $ gui ++ [floatBottom $ widget back, floatBottom $ widget quit]]
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
                   , on command := (close f >> newBoard "Easy" easyH easyW easyM) ]
  medium <- button f [ text := "Medium"
                     , on command := (close f >> newBoard "Medium" mediumH mediumW mediumM) ]
  hard <- button f [ text := "Hard"
                   , on command := (close f >> newBoard "Hard" hardH hardW hardM) ]

  --Frame layout
  set f [layout := minsize (sz 200 100) $ column 1
                   [hfill $ hrule 1
                   ,fill $ margin 10 $ column 1
                   [floatCentre $ widget easy
                   ,floatCentre $ widget medium
                   ,floatCentre $ widget hard
                   ,floatCentre $ widget quit]]]

fieldButton :: Frame () -> Var Meinesweeper -> Field  -> IO (Button ())
fieldButton f gameState field
  | _covered field = makeButton f "  " gameState
  | _flagged field = makeButton f "<|" gameState
  | _mined field   = makeButton f "**" gameState
  | otherwise      = makeButton f "  " gameState
    where
      makeButton f txt g = do
        game <- varGet g
        smallButton f [text := txt
                      ,on click := (\b -> let (x,y) = getPoint b
                                          in varSet g (execState (leftClickField x y) game))
                      ,on clickRight := (\b -> let (x,y) = getPoint b
                                               in varSet g (execState (leftClickField x y) game))
                      ]
      getPoint p = (pointX p, pointY p)

boardGUI :: Board -> Frame () -> Var Meinesweeper -> IO [[Button ()]]
boardGUI b f g = mapM (mapM (fieldButton f g)) (DV.toList $ DV.map DV.toList b)

main :: IO ()
main = start mainMenu
