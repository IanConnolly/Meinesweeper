module Meinesweeper.Board (GameBoard,
                           Board,
                           createBoard,
                           isMined,
                           isCovered,
                           isFlagged,
                           isWon,
                           flag,
                           unflag,
                           uncover,
                           uncoverAll,
                           showBoard) where

import qualified Meinesweeper.Field as MF
import Data.Maybe
import Control.Lens
import Control.Lens.At
import Prelude (($), (.), Bool(..), Int(..), Float(..), const, Num(..), Show(..), String(..), fst, (==), not)
import qualified Data.List as DL
import Data.Vector hiding (modify)
import Control.Monad.State

type Board = Vector (Vector MF.Field)
type GameBoard = State Board
type Height = Int
type Width = Int
type Seed = Float

instance Show Board where
    show :: Board -> String
    show b = shower $ toList $ map toList b
        where shower = DL.foldr (\ b -> (DL.++) (DL.concatMap show b DL.++ "\n")) ""

-- create an initial board
createBoard :: Height -> Width -> Board -- Seed -> Height -> Width -> Board
createBoard h w = insertBombs $ createEmptyBoard h w
    where createEmptyBoard h w = replicate h $ replicate w MF.newField
          insertBombs = map inserter
          inserter = over (element (generateIndex w) . MF.mined) (const True)

generateIndex :: Int -> Int -- TODO: Dan's random index generator
generateIndex x = x - 1

-- Board -> [[Int]]
-- Each elem contains the number of surrounding bombs
-- Or -1 if the elem is itself a bomb
adjacency :: GameBoard [[Int]]
adjacency = do
    board <- get
    let numBoard = numberfiedBoard board
    let zipped = countBombs numBoard
    return $ removeBombSquares zipped numBoard
    where
        numberfiedBoard = toList . map (toList . map (bombToNum . fromJust . preview MF.mined))
        bombToNum a = if a then 1 else 0 
        countBombs = reduceZip . DL.map mapZip
        -- combine surrounding numbers
        -- ie. [1,2,3] -> [3,6,5]
        -- the intermediate steps can be seen as:
        -- [1,2,3] -> [0,1,2,3,0] -> [0+1+2, 1+2+3, 2+3+0] ->[3,6,5] 
        mapZip = zipper add3 0 
        -- 2D (eg. a pointwise) combination, on the above. padding elem is an extra list of zeroes
        reduceZip = zipper (DL.zipWith3 add3) (DL.repeat 0)
        -- higher order abstraction of above
        -- f is combiner, z is padding element
        zipper f z xs = DL.zipWith3 f (z:xs) xs (DL.tail xs DL.++ [z])
        add3 x y z = x + y + z -- convenience
        -- compare each new cell with its original, and change new cell to -1 if a bomb was there
        removeBombSquares new orig = DL.zipWith (DL.zipWith (del)) new orig 
        new `del` orig = if orig == 0 then new else -1

-- check a board for the win condition
isWon :: GameBoard Bool
isWon = do
    board <- get
    let fboard = concat $ toList board
    return $ uncovered fboard == bombs fboard
    where
        uncovered = length . filter (not . fromJust . preview MF.covered)
        bombs = length . filter (fromJust . preview MF.mined)

isMined :: Int -> Int -> GameBoard Bool
isMined x y = viewSquare x y MF.covered

isCovered :: Int -> Int -> GameBoard Bool
isCovered x y = viewSquare x y MF.covered

isFlagged :: Int -> Int -> GameBoard Bool
isFlagged x y = viewSquare x y MF.flagged

uncover :: Int -> Int -> GameBoard ()
uncover x y = modifySquare x y MF.covered False

flag :: Int -> Int -> GameBoard ()
flag x y = modifySquare x y MF.flagged True

unflag :: Int -> Int -> GameBoard ()
unflag x y = modifySquare x y MF.flagged False

uncoverAll :: GameBoard ()
uncoverAll = modifyBoard MF.covered False

getBoard :: GameBoard String
getBoard = do
    board <- get
    return $ show board

showBoard = evalState getBoard

-- set a record of all the squares in the Board to val
modifyBoard record val = modify $ map (map (set record val))

-- set the record of the square at board[x][y] to val
modifySquare x y record val = modify $ over (element x . element y . record) (const val)

-- view the value of the record of the square at board[x][y]
viewSquare x y record = do
    g <- get
    return $ fromJust $ preview (element x . element y . record) g
