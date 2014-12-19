{-# LANGUAGE TemplateHaskell #-}
module Meinesweeper.Field where
import Control.Lens

data Field = Field { _mined :: Bool
                   , _flagged :: Bool
                   , _covered :: Bool
                   , _adjacentMines :: Int
                   }

instance Show Field where
  show :: Field -> String
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
                 }


makeLenses ''Field
