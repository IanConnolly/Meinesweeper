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

newBoard :: String -> Int -> Int -> Int -> IO ()
newBoard title h w m = do
  f <- frameFixed [text := title]
  g <- newStdGen
  gameState <- varCreate $ newMeinesweeper h w m g
  makeGUI gameState f h

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

fieldButton :: Int -> Frame () -> Var Meinesweeper -> Field -> IO (Button ())
fieldButton h f gameState field
  | _flagged field = makeButton h f gameState fId "F"
  | _covered field = makeButton h f gameState fId " "
  | _mined field   = makeButton h f gameState fId "*"
  | otherwise      = makeButton h f gameState fId (show $ _adjacentMines field)
  where
   fId = _fId field
   makeButton h f g i txt = do
     game <- varGet g
     smallButton f [text := txt
                   ,on click := \p -> let (x,y) = getPoint p
                                          (win,state) = runState (leftClickField x y) game
                                      in varSet g state >> putStrLn (show (div i h) ++ " " ++ show (mod i h)) ]{-->> makeGUI g f h]--}
   getPoint p = (pointX p, pointY p)

boardGUI :: Board -> Int -> Frame () -> Var Meinesweeper -> IO [[Button ()]]
boardGUI b h f g = mapM (mapM (fieldButton h f g)) (DV.toList $ DV.map DV.toList b)

widgetise :: Int -> [[Button ()]] -> [Layout]
widgetise _ [] = []
widgetise r (b:bs) = row r (map widget b) : widgetise r bs

makeGUI :: Var Meinesweeper -> Frame () -> Int -> IO ()
makeGUI gameState f h = do
  game <- varGet gameState

  let board = _board game
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