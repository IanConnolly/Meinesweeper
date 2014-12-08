module Meinesweeper.Board (Board, createBoard) where

import Meinesweeper.Field
import qualified Data.List as DL

type Board = [[Field]]
type Height = Int
type Width = Int

instance Show Board where
  show :: Board -> String
  show [] = ""
  show (b:bs) = concatMap show b ++ "\n" ++ show bs

createBoard :: Height -> Width -> Board -- Seed -> Height -> Width -> Board
createBoard h w = insertBombs $ createEmptyBoard h w
    where createEmptyBoard h w = replicate h $ replicate w $ Field { mined = False
                                                                   , flagged = False
                                                                   , covered = True
                                                                   }
          insertBombs = DL.map inserter
          inserter row = let (ys, zs) = splitAt (w - 1) row in -- add in smart function here
                            ys ++ [Field { mined = True, flagged = False, covered = False}] ++ tail zs
