{-# LANGUAGE TemplateHaskell #-}
module Meinesweeper.Field where
import Control.Lens
import Graphics.UI.WX (Frame, button, Button, text, Prop(..))

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

fieldButton :: Frame () -> Field -> IO (Button ())
fieldButton frame field
  | _flagged field = makeButton frame "<|"
  | _covered field = makeButton frame "  "
  | _mined field   = makeButton frame "**"
  | otherwise      = makeButton frame "  "
    where
      makeButton f txt = button f [text := txt]

newField :: Field
newField = Field { _mined = False
                 , _flagged = False
                 , _covered = True
                 }

makeLenses ''Field
