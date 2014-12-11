module Meinesweeper.Game where

import Meinesweeper.Board
import Meinesweeper.Field
import Control.Monad.State
import Data.Vector as DV
import Data.Maybe
import Control.Lens

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

isWon :: GameBoard Bool
isWon = do
    board <- get
    let fboard = DV.concat $ DV.toList board
    return $ uncovered fboard == bombs fboard
    where
        uncovered l = DV.length $ DV.filter (not . fromJust . preview covered) l
        bombs l = DV.length $ DV.filter (fromJust . preview mined) l