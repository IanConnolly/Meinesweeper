module Meinesweeper.Game where

import Meinesweeper.Board
import Control.Monad.State

--newtype Game = State GameBoard
type Height = Int
type Width = Int

{--instance Monad Game where
    return = undefined
    (>>=) = undefined--}

startGame :: Height -> Width -> GameBoard ()
startGame h w = put $ createBoard h w

clickField :: Int -> Int -> GameBoard ()
clickField x y = do
    c <- isCovered x y
    if c then checkForMine x y else doNothing

rightClickField :: Int -> Int -> GameBoard ()
rightClickField x y = do
    f <- isFlagged x y
    if f then flag x y else unflag x y

checkForMine :: Int -> Int -> GameBoard ()
checkForMine x y = do
    m <- isMined x y
    if m then uncoverAll else uncover x y

showBoard :: String
showBoard = fst $ runState getBoard $ createBoard 4 4
