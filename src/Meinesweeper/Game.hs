{-# LANGUAGE TemplateHaskell #-}
module Meinesweeper.Game where

import Meinesweeper.Board
import qualified Meinesweeper.Field as MF
import Prelude (Bool(..), Int(..), Num(..), Show(..), String(..),
                const, (==), not, ($), (.), unlines)
import Control.Monad
import Control.Monad.State
import Control.Lens
import Data.Vector hiding (modify)
import Data.Maybe
import System.Random
import qualified Data.List as DL

data Meinesweeper = Meinesweeper
    { _flagsLeft :: Int
    , _board :: Board
    }

makeLenses ''Meinesweeper

instance Show Meinesweeper where
    show b = unlines $ [show $ b ^. board,
                        "Flags: " DL.++ (show (b ^. flagsLeft))]
 
type Game = State Meinesweeper

newMeinesweeper :: Int -> Int -> Int -> StdGen -> Meinesweeper
newMeinesweeper h w m g =
  Meinesweeper { _flagsLeft = m
               , _board = createBoard h w m g
               }

leftClickField :: Int -> Int -> Game Bool
leftClickField x y = do
    c <- isCovered x y
    if c then do
        m <- isMined x y
        if m then do
            uncoverAll
            return False
        else do
            uncover x y
            return True
    else
        return True

rightClickField :: Int -> Int -> Game Bool
rightClickField x y = do
    game <- get
    f <- isFlagged x y
    if f then
        if (game ^. flagsLeft) == 0 then
            return False
        else do
            flag x y
            flagsLeft -= 1
            return True
    else do
        unflag x y
        flagsLeft += 1
        return True

-- check a board for the win condition
isWon :: Game Bool
isWon = do
    game <- get
    let b = fromJust $ preview board game
    let fboard = concat $ toList b
    return $ uncovered fboard == mines fboard
    where
        uncovered = length . filter (not . fromJust . preview MF.covered)
        mines = length . filter (fromJust . preview MF.mined)

isMined :: Int -> Int -> Game Bool
isMined x y = viewSquare x y MF.covered

isCovered :: Int -> Int -> Game Bool
isCovered x y = viewSquare x y MF.covered

isFlagged :: Int -> Int -> Game Bool
isFlagged x y = viewSquare x y MF.flagged

adjacentMines :: Int -> Int -> Game Int
adjacentMines x y = viewSquare x y MF.adjacentMines

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
    modify $ over board (map (map (set record val)))

-- set the record of the square at board[x][y] to val
modifySquare x y record val = modify $ over (board . element x . element y . record) (const val)

-- view the value of the record of the square at board[x][y]
viewSquare x y record = do
    g <- get
    return $ fromJust $ preview (board . element x . element y . record) g
