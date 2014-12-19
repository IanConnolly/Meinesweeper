module Meinesweeper.Board where

import Prelude (Bool(..), Int(..), Num(..), Show(..), String(..),
                const, (==), not, ($), (.)) 
import Control.Lens
import System.Random
import Data.Tuple
import Data.Maybe
import Data.Vector hiding (modify)
import qualified Data.List as DL
import qualified Meinesweeper.Field as MF

type Board = Vector (Vector MF.Field)
type Height = Int
type Width = Int

instance Show Board where
    show :: Board -> String
    show b = shower $ toList $ map toList b
        where shower = DL.foldr (\ b -> (DL.++) (DL.concatMap show b DL.++ "\n")) ""

-- create an initial board
createBoard :: Height -> Width -> Int -> StdGen -> Board
createBoard h w mcount prng = 
    let b = insertMines points $ createEmptyBoard h w
    in addAdjacent b (computeAdjacencyMatrix b)                              
    where createEmptyBoard h w = replicate h $ replicate w MF.newField
          points = DL.take mcount $ DL.nub $ DL.zip xCoords yCoords
          xCoords = randomRs (0, w-1) (fst $ split prng)
          yCoords = randomRs (0, h-1) (snd $ split prng)

group :: Int -> [a] -> [[a]]
group n [] = []
group n l = first : group n rest
      where (first, rest) = DL.splitAt n l

addAdjacent :: Board -> [[Int]] -> Board
addAdjacent b adjs = boardify $ addNums union
    where boardify = map fromList . fromList . group (length $ b ! 0)
          addNums = DL.map addNum
          addNum (cell, num) = set MF.adjacentMines num cell
          union = DL.zip (DL.concatMap toList (toList b)) (DL.concat adjs)

insertMines :: [(Int, Int)] -> Board -> Board
insertMines [] board = board
insertMines (x:xs) board = insertMines xs (inserter x board)
    where inserter (x, y) = over (element x . element y . MF.mined) (const True)

-- Board -> [[Int]]
-- Each elem contains the number of surrounding Mines
-- Or -1 if the elem is itself a bomb
computeAdjacencyMatrix :: Board -> [[Int]]
computeAdjacencyMatrix = countMines . numberfiedBoard
    where
        numberfiedBoard = toList . map (toList . map (bombToNum . fromJust . preview MF.mined))
        bombToNum a = if a then 1 else 0
        countMines = reduceZip . DL.map mapZip
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