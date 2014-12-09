module Meinesweeper.Game where

import Meinesweeper.Board
import Control.Monad.State

type Height = Int
type Width = Int

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
