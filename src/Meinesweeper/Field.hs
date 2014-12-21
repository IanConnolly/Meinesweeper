{-# LANGUAGE TemplateHaskell #-}
module Meinesweeper.Field where
import Control.Lens

data Field = Field { _mined :: Bool
                   , _flagged :: Bool
                   , _covered :: Bool
                   , _adjacentMines :: Int
                   , _xy :: (Int,Int)
                   }

instance Show Field where
  show f
    | _flagged f = "| F |"
    | _covered f = "|   |"
    | _mined f   = "| * |"
    | otherwise  = "| " ++ show (_adjacentMines f) ++ " |"

newField :: Field
newField = Field { _mined = False
                 , _flagged = False
                 , _covered = True
                 , _adjacentMines = 0
                 , _xy = (-1,-1)
                 }


makeLenses ''Field
