module Meinesweeper.Board (Board, createBoard, flagSquare) where

import Meinesweeper.Field
import Data.Maybe
import Control.Lens
import Control.Lens.At
import Prelude (($), (.), Bool(..), Int(..), const, Num(..), Show(..), String(..))
import qualified Data.List as DL
import Data.Vector hiding (modify)
import Control.Monad.State 

type Board = Vector (Vector Field)
type GameBoard = State Board
type Height = Int
type Width = Int

instance Show Board where
    show :: Board -> String
    show b = shower $ toList $ map toList b
        where shower [] = ""
              shower (b:bs) = DL.concatMap show b DL.++ "\n" DL.++ (shower bs)


createBoard :: Height -> Width -> Board -- Seed -> Height -> Width -> Board
createBoard h w = insertBombs $ createEmptyBoard h w
    where createEmptyBoard h w = replicate h $ replicate w $ newField
          insertBombs = map inserter
          inserter = over (element (generateIndex w) . mined) (const True)

generateIndex :: Int -> Int
generateIndex x = x - 1

flagSquare :: Int -> Int -> GameBoard ()
flagSquare x y = modify $ over (element y . element x . flagged) (const True)

unflagSquare :: Int -> Int -> GameBoard ()
unflagSquare x y = modify $ over (element y . element x . flagged) (const False)