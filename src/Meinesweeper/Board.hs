module Meinesweeper.Board (Board, createBoard) where

import Meinesweeper.Field (Field (Field, mined, flagged, covered))
import qualified Data.List as DL

type Board = [[Field]]
type Height = Int
type Width = Int


createBoard :: Height -> Width -> Board -- Seed -> Height -> Width -> Board
createBoard h w = insertBombs $ createEmptyBoard h w
    where createEmptyBoard h w = replicate h $ replicate w $ Field { mined = False
                                                                   , flagged = False
                                                                   , covered = False 
                                                                   }
          insertBombs = DL.map inserter
          inserter row = let (ys, zs) = splitAt (w - 1) row in -- add in smart function here
          					 ys ++ [Field { mined = True, flagged = False, covered = False}] ++ zs
