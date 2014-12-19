{-# LANGUAGE TemplateHaskell #-}
module Meinesweeper.Game where

import Meinesweeper.Board
import qualified Meinesweeper.Field as MF
import Prelude (Bool(..), Int(..), Num(..), Show(..), String(..),
                const, (==), not, ($), (.)) 
import Control.Monad
import Control.Monad.State
import Control.Lens
import Data.Vector hiding (modify)
import Data.Maybe

data Meinesweeper = Meinesweeper
    { _flagsLeft :: Int
    , _board :: Board
    }

makeLenses ''Meinesweeper

type Game = State Meinesweeper

leftClickField :: Int -> Int -> Game ()
leftClickField x y = do
    c <- isCovered x y
    when c $ do
        m <- isMined x y
        if m
            then uncoverAll
            else uncover x y

rightClickField :: Int -> Int -> Game ()
rightClickField x y = do
    f <- isFlagged x y
    if f
        then do
            flag x y
            flagsLeft -= 1
        else do
            unflag x y
            flagsLeft += 1

-- check a board for the win condition
isWon :: Game Bool
isWon = do
    game <- get
    let b = fromJust $ preview board game
    let fboard = concat $ toList b
    return $ uncovered fboard == bombs fboard
    where
        uncovered = length . filter (not . fromJust . preview MF.covered)
        bombs = length . filter (fromJust . preview MF.mined)

isMined :: Int -> Int -> Game Bool
isMined x y = viewSquare x y MF.covered

isCovered :: Int -> Int -> Game Bool
isCovered x y = viewSquare x y MF.covered

isFlagged :: Int -> Int -> Game Bool
isFlagged x y = viewSquare x y MF.flagged

uncover :: Int -> Int -> Game ()
uncover x y = modifySquare x y MF.covered False

flag :: Int -> Int -> Game ()
flag x y = modifySquare x y MF.flagged True

unflag :: Int -> Int -> Game ()
unflag x y = modifySquare x y MF.flagged False

uncoverAll :: Game ()
uncoverAll = modifyBoard MF.covered False

-- set a record of all the squares in the Board to val
modifyBoard record val = 
    modify $ over (board) (map (map (set record val)))

-- set the record of the square at board[x][y] to val
modifySquare x y record val = modify $ over (board . element x . element y . record) (const val)

-- view the value of the record of the square at board[x][y]
viewSquare x y record = do
    g <- get
    return $ fromJust $ preview (board . element x . element y . record) g

