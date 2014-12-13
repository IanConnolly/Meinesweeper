{-# LANGUAGE TemplateHaskell #-}
module Meinesweeper.Game where

import Meinesweeper.Board
import Control.Monad.State
import Control.Lens

data Meinesweeper = Meinesweeper
    { _flagsLeft :: Int
    }

makeLenses ''Meinesweeper

type Game = StateT Meinesweeper (GameBoard) 

leftClickField :: Int -> Int -> Game ()
leftClickField x y = do
    c <- lift $ isCovered x y
    if c 
        then lift $ do
            m <- isMined x y
            if m 
                then uncoverAll 
                else uncover x y
        else return ()

rightClickField :: Int -> Int -> Game ()
rightClickField x y = do
    f <- lift $ isFlagged x y
    if f 
        then do
            lift $ flag x y 
            flagsLeft -= 1
        else do
            lift $ unflag x y
            flagsLeft += 1
