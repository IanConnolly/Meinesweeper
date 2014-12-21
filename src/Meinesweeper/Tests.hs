module Main where

import Meinesweeper.Board
import Test.QuickCheck

main  = mapM_ (\(s ,a) -> print s >> a) tests
 
-- reversing twice a finite list, is the same as identity
prop_reversereverse s = (reverse . reverse) s == id s
    where _ = s :: [Int]
 
tests  = [("reverse.reverse/id", quickCheck prop_reversereverse)]