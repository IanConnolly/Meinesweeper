module Main where

import Meinesweeper.CLI
import Meinesweeper.Graphics
import System.IO
import System.Environment


usage = unlines $ ["usage: meinesweeper [options]",
                   "\toptions:",
                   "\t\t--gui: run in gui mode",
                   "\t\t--cli: run in cli mode",
                   "\t\t-h, --help: print this message"]

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  args <- getArgs

  case length args of
    0 -> startCLILoop
    1 -> do
        case head args of
            "--gui" -> startGraphicsLoop
            "--cli" -> startCLILoop
            "-h" -> putStrLn usage
            "--help" -> putStrLn usage
            otherwise -> do 
                putStrLn $ "Unknown argument: " ++ (show $ head args)
                putStrLn usage
    otherwise -> do
        putStrLn "Saw >1 args, expecting 0 or 1"
        putStrLn usage
