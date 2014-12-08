{-# LANGUAGE TemplateHaskell #-}
module Meinesweeper.Field where
import Control.Lens

data Field = Field { _mined :: Bool
                   , _flagged :: Bool
                   , _covered :: Bool
                   }

instance Show Field where
  show :: Field -> String
  show f =
    if _flagged f then "| <| |" else
       if _covered f then "| [] |" else
          if _mined f then "| * |" else "| |"

newField :: Field
newField = Field { _mined = False
				 , _flagged = False
				 , _covered = True
				 }

makeLenses ''Field
