module Meinesweeper.Board (GameBoard,
                           Board,
                           createBoard,
                           isMined,
                           isCovered,
                           isFlagged,
                           uncover,
                           flag,
                           unflag,
                           doNothing,
                           uncoverAll,
                           showBoard) where

import qualified Meinesweeper.Field as MF
import Data.Maybe
import Control.Lens
import Control.Lens.At
import Prelude (($), (.), Bool(..), Int(..), const, Num(..), Show(..), String(..), fst)
import qualified Data.List as DL
import Data.Vector hiding (modify)
import Control.Monad.State

type Board = Vector (Vector MF.Field)
type GameBoard = State Board
type Height = Int
type Width = Int

instance Show Board where
    show :: Board -> String
    show b = shower $ toList $ map toList b
        where shower = foldr (\ b -> (DL.++) (DL.concatMap show b DL.++ "\n")) ""


createBoard :: Height -> Width -> Board -- Seed -> Height -> Width -> Board
createBoard h w = insertBombs $ createEmptyBoard h w
    where createEmptyBoard h w = replicate h $ replicate w MF.newField
          insertBombs = map inserter
          inserter = over (element (generateIndex w) . MF.mined) (const True)

generateIndex :: Int -> Int -- TODO: Dan's random index generator
generateIndex x = x - 1

isMined :: Int -> Int -> GameBoard Bool
isMined x y = viewSquare x y MF.covered

isCovered :: Int -> Int -> GameBoard Bool
isCovered x y = viewSquare x y MF.covered

isFlagged :: Int -> Int -> GameBoard Bool
isFlagged x y = viewSquare x y MF.flagged

uncover :: Int -> Int -> GameBoard ()
uncover x y = modifySquare x y MF.covered False

flag :: Int -> Int -> GameBoard ()
flag x y = modifySquare x y MF.flagged True

unflag :: Int -> Int -> GameBoard ()
unflag x y = modifySquare x y MF.flagged False

uncoverAll :: GameBoard ()
uncoverAll = modifyBoard MF.covered False

doNothing :: GameBoard ()
doNothing = return ()

getBoard :: GameBoard String
getBoard = do
    board <- get
    return $ show board

showBoard = evalState getBoard

modifyBoard record val = modify $ map (map (set record val))

modifySquare x y record val = modify $ over (element y . element x . record) (const val)

viewSquare x y record = do
    g <- get
    return $ fromJust $ preview (element y . element x . record) g
