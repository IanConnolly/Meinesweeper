module Meinesweeper.Field (Field (Field, mined, flagged, covered)) where

data Field = Field { mined :: Bool
                   , flagged :: Bool
                   , covered :: Bool
                   } deriving (Show)