module Main where

import Meinesweeper.Board
import Meinesweeper.Field
import Meinesweeper.Game
import Control.Lens
import Control.Monad.State
import Test.QuickCheck
import qualified Data.Vector as DV
import qualified Data.List as DL

newtype Location = L Int

main  = mapM_ (\(s, a) -> print s >> a) tests
 
prop_matrixsize b = (DL.length $ DL.concat $ computeAdjacencyMatrix b) == (DV.length $ DV.concat $ DV.toList b)
    where _ = b :: Board

prop_rightclickstep xs@(L x) ys@(L y) b = 
    let (val, newstate) = runState (rightClickField x y) b
    in (_flagsLeft newstate) < (_flagsLeft b)
    where _ = xs :: Location
          _ = ys :: Location
          _ = b :: Meinesweeper

instance Arbitrary Location where
    arbitrary = return $ L 0

instance Show Location where
    show (L x) = show x 

instance Arbitrary Field where
    arbitrary = return $ newField

instance Arbitrary Board where
    arbitrary = do
        Positive h <- arbitrary
        Positive w <- arbitrary
        nf <- arbitrary
        return $ DV.replicate h $ DV.replicate w nf

instance Arbitrary Meinesweeper where
    arbitrary = do
        Positive flags <- arbitrary
        b <- arbitrary
        return $ Meinesweeper { _flagsLeft = flags 
                              , _board = b
                              }
 
tests  = [("prop_matrixsize", quickCheck prop_matrixsize)
         ,("prop_rightclickstep", quickCheck prop_rightclickstep)]