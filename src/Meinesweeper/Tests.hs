module Main where

import Meinesweeper.Board
import Meinesweeper.Field
import Test.QuickCheck
import qualified Data.Vector as DV
import qualified Data.List as DL

main  = mapM_ (\(s, a) -> print s >> a) tests
 
prop_matrixsize b = (DL.length $ DL.concat $ computeAdjacencyMatrix b) == (DV.length $ DV.concat $ DV.toList b)
    where _ = b :: Board

instance Arbitrary Field where
    arbitrary = return $ newField

instance Arbitrary Board where
    arbitrary = do
        Positive h <- arbitrary
        Positive w <- arbitrary
        nf <- arbitrary
        return $ DV.replicate h $ DV.replicate w nf
 
tests  = [("prop_matrixsize", quickCheck prop_matrixsize)]