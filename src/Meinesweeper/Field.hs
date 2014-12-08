module Meinesweeper.Field where

data Field = Field { mine :: Bool
                   , flagged :: Bool
                   , covered :: Bool
                   } deriving (Show)