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


startCLILoop = initGame
winMessage = "You won!"
loseMessage = "You lost!"
outOfFlags = "You're out of flags :("

data Difficulty = Easy | Medium | Hard deriving (Enum, Show)

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


gameLoop :: Meinesweeper -> IO Bool -- TODO, transform this malarky
gameLoop state = do
    print state
    inputs <- inputPrompt
    case head inputs of
        "f" -> do
            let x = digitToInt $ (inputs !! 1) !! 0
            let y = digitToInt $ (inputs !! 2) !! 0
            let (res, newstate) = runState (rightClickField x y) state
            if res then
                gameLoop newstate
            else do
                putStrLn outOfFlags
                gameLoop newstate
        otherwise -> do 
            let x = digitToInt $ (inputs !! 0) !! 0
            let y = digitToInt $ (inputs !! 1) !! 0
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
    return $ toEnum $ (digitToInt $ input !! 0) - 1


inputPrompt :: IO [String]
inputPrompt = do
    putStrLn "Commands:"
    putStrLn "f x y -> Flag cell (x,y)"
    putStrLn "x y -> Uncover cell (x,y)"
    inp <- prompt "> "
    return $ words inp

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
    return $ s