module Meinesweeper.Field where

data Field = Field { mined :: Bool
                   , flagged :: Bool
                   , covered :: Bool
                   }

instance Show Field where
  show :: Field -> String
  show f =
    if flagged f then "| <| |" else
       if covered f then "| [] |" else
          if mined f then "| * |" else "| |"
