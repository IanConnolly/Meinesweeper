{-# LANGUAGE TemplateHaskell #-}
module Meinesweeper.Game where

import Meinesweeper.Board
import Control.Monad
import Control.Monad.State
import Control.Lens
import Graphics.UI.WX (Frame, Button)

data Meinesweeper = Meinesweeper
    { _flagsLeft :: Int
    }

makeLenses ''Meinesweeper

type Game = StateT Meinesweeper GameBoard

leftClickField :: Int -> Int -> Game ()
leftClickField x y = do
    c <- lift $ isCovered x y
    when c $
        lift $ do
            m <- isMined x y
            if m
                then uncoverAll
                else uncover x y

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

createBoard' = createBoard

boardGUI' :: Board -> Frame () -> IO [[Button ()]]
boardGUI' = boardGUI
