module Meinesweeper.Solver where

import Meinesweeper.Board
import qualified Meinesweeper.Field as MF
import Control.Monad
import Control.Monad.State
import Control.Lens
import qualified Data.Vector as DV
import Data.Matrix

-- Rename createAdjacencyMatrix to calculateAdjacencies
-- TODO: Write solutionMatrix generator

-- Working from algorithm detailed here: http://quantum-p.livejournal.com/19616.html


initSolverMatrix :: Game -> Matrix
initSolverMatrix game = 
where 
      board = evalState -- or something like that idfk
      adjacencies = calculateAdjacencies board
      listedAdj = filter (<1) $ map (++) adjacencies

reducedRowEchelon :: Num a => Matrix a -> Bool
reducedRowEchelon m
	| (diagProd m == 1 && trace m == nrows m && sumUnderDiagonal == 0) = True
	| otherwise                                                        = False


-- Takes params in a non-intuitive order because they always take the Matrix last in Data.Matrix functions
-- Start range at 1 or 0? How are matrices indexed?
sumUnderDiagonal :: Num a => Matrix a -> a
sumUnderDiagonal m =  sum $ map $ ((flip sumColBelowDiag) m) [0..(ncols m) - 1]

sumColBelowDiag :: Num a => Int -> Matrix a -> a
sumColBelowDiag c m = DV.sum $ DV.drop c $ getCol c m

-- Elementary row operations
-- scaleRow :: Num a => a -> Int -> Matrix a -> Matrix a
-- switchRows :: Int -> Int -> Matrix a -> Matrix a
-- combineRows :: Num a => Int -> a -> Int -> Matrix a -> Matrix a

performERO :: Num a => Matrix a -> Matrix a
performERO m
  -- Just need to sort out column 0 here - the rest will be sorted out by recursively doing this to submatrices
  |	m ! (0,0) == 0 = 							-- combineRows step if leading 0
  | m ! (0,0) != 1 = scaleRow 0 (1 / (m ! (0,0) )) -- sort out row 0
  |												   -- general combineRows step (to all remaining lines)
  | otherwise      = performERO (minorMatrix 0 0 m) -- Needs to combine with row 0 and col 0 to finish


