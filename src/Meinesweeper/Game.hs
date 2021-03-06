{-# LANGUAGE TemplateHaskell #-}
module Meinesweeper.Game (Meinesweeper(..), Game(..), newMeinesweeper,
                          leftClickField, rightClickField, isWon, solveStep) where

import Meinesweeper.Board
import qualified Meinesweeper.Field as MF
import Prelude (Bool(..), Int(..), Num(..), Show(..), String(..), otherwise,
                const, (==), (>=), (<), (&&), not, ($), (.), unlines, uncurry)
import Control.Monad hiding (sequence_)
import Control.Monad.State
import Control.Lens
import Data.Vector hiding (modify, sequence_)
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
    if f then do
        unflag x y
        flagsLeft += 1
        return True
    else
        if (game ^. flagsLeft) == 0 then
            return False
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
-- If an uncovered square has mine-adjacency = num adjacent covered{
--    flag unflagged covered adjacents
-- }
--
-- Uncover criterion:
-- If an uncovered square has mine-adjacency = num adjacent flagged {
--    uncover all adjacents not flagged
-- }

type Coord = (Int, Int)

solveFlag :: Meinesweeper -> Coord -> [Game ()]
solveFlag game (x, y) =
    let b = fromJust $ preview board game
        adjancies = adjacents (x, y) b
        covered = coveredAdjacents adjancies game
        flagged = flaggedAdjacents adjancies game
    in if not (evalState (isCovered x y) game) && (adjacency b == DL.length covered)
          then DL.map (uncurry flag) (covered DL.\\ flagged)
          else []
    where
        adjacency = fromJust . preview (element x . element y . MF.adjacentMines)
        flaggedAdjacents a g = DL.filter isCoord $ DL.map (adjacentCoords isFlagged g) a
        coveredAdjacents a g = DL.filter isCoord $ DL.map (adjacentCoords isCovered g) a

solveUncover :: Meinesweeper -> Coord -> [Game ()]
solveUncover game (x, y) =
  let b = fromJust $ preview board game
      adjancies = adjacents (x, y) b
      covered = coveredAdjacents adjancies game
      flagged = flaggedAdjacents adjancies game
  in
  if not (evalState (isCovered x y) game) && (adjacency b == DL.length flagged)
        then DL.map (uncurry uncover) (covered DL.\\ flagged)
  else []
    where
        adjacency = fromJust . preview (element x . element y . MF.adjacentMines)
        flaggedAdjacents a g = DL.filter isCoord $ DL.map (adjacentCoords isFlagged g) a
        coveredAdjacents a g = DL.filter isCoord $ DL.map (adjacentCoords isCovered g) a

--For use in filtering Coords produced by adjacentCoords
isCoord :: Coord -> Bool
isCoord (x, y) = not (x == -1 && y == -1)

adjacentCoords :: (Int -> Int -> Game Bool) -> Meinesweeper -> Coord -> Coord
adjacentCoords action g (x,y) =
    let pred = evalState (action x y) g
    in if pred then (x, y)
               else (-1, -1)

--solveFlag across board, then solveUncover across board
solveStep :: Game ()
solveStep = do
    game <- get
    let b = fromJust $ preview board game
    let xs = [0..length b - 1]
    let ys = [0..length (b ! 0) - 1]
    let coords = [(x,y) | x <- xs, y <- ys]
    let flagsActions = DL.map (solveFlag game) coords
    let uncoverActions = DL.map (solveUncover game) coords
    let flagsActions' = DL.concat flagsActions
    let uncoverActions' = DL.concat uncoverActions
    let completeSolveStep = flagsActions' DL.++ uncoverActions'
    runall completeSolveStep

-- Find set of adjacent squares
adjacents :: Coord -> Board -> [Coord]
adjacents (x,y) board = DL.filter inBounds [(x - 1, y - 1), (x - 1, y)
                                           ,(x - 1, y + 1), (x, y - 1)
                                           ,(x, y + 1), (x + 1, y - 1)
                                           ,(x + 1, y), (x + 1,y + 1)]
    where inBounds (x0, y0) = (x0 >= 0 && x0 < nrows) && (y0 >= 0 && y0 < ncols)
          ncols = length (board ! 0)
          nrows = length board

runall :: [Game ()] -> Game ()
runall [] = return ()
runall (x:xs) = do
    g <- get
    let g' = execState x g
    put g'
    runall xs
