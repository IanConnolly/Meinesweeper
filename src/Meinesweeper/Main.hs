module Main where

import Meinesweeper.CLI
import System.IO

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  startCLILoop
