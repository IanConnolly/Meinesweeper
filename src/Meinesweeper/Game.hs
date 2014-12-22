{-# LANGUAGE TemplateHaskell #-}
module Meinesweeper.Game (Meinesweeper(..), Game(..), newMeinesweeper,
                          leftClickField, rightClickField, isWon) where

import Meinesweeper.Board
import qualified Meinesweeper.Field as MF
import Prelude (Bool(..), Int(..), Num(..), Show(..), String(..),
                const, (==), (>=), (<), (&&), not, ($), (.), unlines, uncurry)
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
    show b = unlines [show $ b ^. board,
                      "Flags: " DL.++ show (b ^. flagsLeft)]
 
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
            unflag x y
            flagsLeft += 1
            return True
    else do
        flag x y
        flagsLeft -= 1
        return True

-- check a board for the win condition
-- Win condition: The amount of covered fields remaining == num of mines
isWon :: Game Bool
isWon = do
    game <- get
    let b = fromJust $ preview board game
    let fboard = concat $ toList b
    return $ covered fboard == mines fboard
    where
        covered = length . filter (fromJust . preview MF.covered)
        mines = length . filter (fromJust . preview MF.mined)

isMined :: Int -> Int -> Game Bool
isMined x y = viewSquare x y MF.mined

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


-- Simple solver
-- Algorithm:
--
-- Flag criterion:
-- If an uncovered square has mine-adjacency = num adjacent covered and not flagged + num adjacent flagged {
--    flag adjacent covered
-- }
--
-- Uncover criterion:
-- If an uncovered square has mine-adjacency = num adjacent flagged {
--    uncover all adjacents not flagged
-- }

type Coord = (Int,Int)

solveFlag :: Coord -> Game ()
solveFlag (x,y) = do
    game <- get
    let b = fromJust $ preview board game
    let adjacency = ((computeAdjacencyMatrix b) DL.!! y) DL.!! x
    let flaggedAdjacents = DL.filter (uncurry isFlagged) (adjacents (x,y) b)
    let coveredAdjacents = DL. filter (uncurry (not isFlagged)) $ DL.filter (uncurry isCovered) (adjacents (x,y) b)
    if (not isCovered x y && adjacency == length coveredAdjacents + length flaggedAdjacents) then 
            DL.map (uncurry flag) coveredAdjacents
    else return

solveClear :: Coord -> Game ()
solveClear (x,y) = do
    game <- get
    let b = fromJust $ preview board game
    let adjacency = ((computeAdjacencyMatrix b) DL.!! y) DL.!! x
    let flaggedAdjacents = DL.filter (uncurry isFlagged) (adjacents (x,y) b)
    let coveredAdjacents = DL. filter (uncurry (not isFlagged)) $ DL.filter (uncurry isCovered) (adjacents (x,y) b)
    if (not isCovered x y && adjacency == length flaggedAdjacents) then
        DL.map (uncurry uncover) coveredAdjacents
    else return   

--solveFlag across board THEN solveClear
solveStep :: Game ()
solveStep = do
    game <- get
    let b = fromJust $ preview board game
    let xs = [0..(length (b ! 0))-1]
    let ys = [0..length b-1]
    let coords = [(x,y) | x <- xs, y <- ys]
    DL.map solveFlag coords
    DL.map solveClear coords

-- Find set of adjacent squares
adjacents :: Coord -> Board -> [Coord]
adjacents (x,y) board = DL.filter (inBounds) [(x-1,y-1),(x-1,y),(x-1,y+1),(x,y-1),(x,y+1),(x+1,y-1),(x+1,y),(x+1,y+1)]
    where   inBounds (x0,y0) = (x0>=0 && x0<ncols) && (y0>=0 && y0<nrows)
            nrows = length board
            ncols = length (board ! 0)
