module Meinesweeper.CLI (startCLILoop) where

import Meinesweeper.Game
import Meinesweeper.Board
import Meinesweeper.Field
import Meinesweeper.Constants
import System.Random
import System.Exit
import System.IO
import Control.Monad.State
import Data.Char

startCLILoop :: IO () -- entry point for export
startCLILoop = initGame

winMessage = "You won!"
loseMessage = "You lost!"
outOfFlags = "You're out of flags :("

data Difficulty = Easy | Medium | Hard 
    deriving (Enum, Show) -- helper type for nice newGame pattern matching

initGame :: IO ()
initGame = do
    difficulty <- difficultyPrompt
    prng <- newStdGen
    let game = newGame difficulty prng
    won <- gameLoop game
    if won then 
        putStrLn winMessage
    else 
        putStrLn loseMessage

-- Main game interaction loop
-- returns True if won, False if lost
gameLoop :: Meinesweeper -> IO Bool
gameLoop state = do
    print state
    inputs <- inputPrompt
    case head inputs of
        "f" -> do
            let x = (read $ inputs !! 1) :: Int
            let y = (read $ inputs !! 2) :: Int
            let (res, newstate) = runState (rightClickField x y) state
            if res then
                gameLoop newstate
            else do
                putStrLn outOfFlags
                gameLoop newstate

        "s" -> do
            let (won, newstate) = runState (solveStep >> isWon) state
            if won then
                return True
            else
                gameLoop newstate
        otherwise -> do 
            let x = (read $ inputs !! 0) :: Int 
            let y = (read $ inputs !! 1) :: Int
            let (res, newstate) = runState (leftClickField x y) state
            if res then do
                let won = evalState isWon newstate
                if won then
                    return True
                else
                    gameLoop newstate
            else do
                print newstate
                return False


difficultyPrompt :: IO Difficulty
difficultyPrompt = do
    putStrLn $ unlines ["1) Easy", "2) Medium", "3) Hard"]
    input <- prompt "Enter difficulty: "
    return $ toEnum $ digitToInt (head input) - 1


inputPrompt :: IO [String]
inputPrompt = do
    putStrLn "Commands:"
    putStrLn "x y -> Uncover cell (x,y)"
    putStrLn "f x y -> Flag cell (x,y)"
    putStrLn "s -> cheat! take a solve step"
    inp <- prompt "> "
    return $ words inp

-- dispatch correct args to newMeinesweeper
newGame :: Difficulty -> StdGen -> Meinesweeper
newGame Easy g = newMeinesweeper easyH easyW easyM g
newGame Medium g = newMeinesweeper mediumH mediumW mediumM g
newGame Hard g = newMeinesweeper hardH hardW hardM g


prompt :: String -> IO String
prompt p = do
    putStr p
    hFlush stdout -- else the putStr will be buffered & not output til later
    s <- getLine
    putStr "\n"
    return s