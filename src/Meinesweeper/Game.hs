{-# LANGUAGE TemplateHaskell #-}
module Meinesweeper.Game where

import Meinesweeper.Board
import Meinesweeper.Field
import Control.Monad.State
import qualified Data.Vector as DV
import Data.Maybe
import Control.Lens

type Height = Int
type Width = Int

data Meinesweeper = Meinesweeper
    { _flagsLeft :: Int
    }

makeLenses ''Meinesweeper

type Game = StateT Meinesweeper (GameBoard) 

clickField :: Int -> Int -> Game ()
clickField x y = do
    c <- lift isCovered x y
    if c then lift checkForMine x y else return ()

rightClickField :: Int -> Int -> Game ()
rightClickField x y = do
    f <- lift isFlagged x y
    if f then lift flag x y else lift unflag x y

checkForMine :: Int -> Int -> Game ()
checkForMine x y = do
    m <- lift isMined x y
    if m then lift $ uncoverAll else lift uncover x y

isWon :: GameBoard Bool
isWon = do
    board <- get
    let fboard = DV.concat $ DV.toList board
    return $ uncovered fboard == bombs fboard
    where
        uncovered = DV.length . DV.filter (not . fromJust . preview covered)
        bombs = DV.length . DV.filter (fromJust . preview mined)
