{-# LANGUAGE TemplateHaskell #-}
module Meinesweeper.Field where
import Control.Lens

data Field = Field { _mined :: Bool
                   , _flagged :: Bool
                   , _covered :: Bool
                   }

instance Show Field where
  show :: Field -> String
  show f
    | _flagged f = "|  <|  |"
    | _covered f = "|  []  |"
    | _mined f   = "|  **  |"
    | otherwise  = "|      |"

newField :: Field
newField = Field { _mined = False
                 , _flagged = False
                 , _covered = True
                 }

makeLenses ''Field
