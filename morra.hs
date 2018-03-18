-- Morra
-- >= 2 Players
-- Each player -> 0 <= x <= 5 fingers
-- Each player calls out guess for the sum of the fingers
-- Players that guess correctly are awarded a point
-- First player to 3 is the winner

-- Morra

-- 0..5: 3
-- sum: 5

-- P1: 3 | 5
-- AI: 2 | 6

-- Score: 0, 1

-- Winner: P1

{-# LANGUAGE MultiWayIf #-}

module Morra where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.State
import qualified Data.Map                  as M
import           Data.Monoid
import           Prelude                   hiding (round)

data PlayerTypes = AI | P1 deriving (Eq, Ord, Show)

data Hand = Zero | One | Two | Three | Four | Five deriving (Enum, Show)
type Guess = Integer
data Move = Move Hand Guess
type Moves = (Move, Move)

data GameState = GameState { gScores :: M.Map PlayerTypes Integer }

type Morra = StateT GameState IO Moves

playerInput :: IO Move
playerInput = do
  putStr "\n0..5: "
  hand <- getLine
  putStr "sum: "
  guess <- getLine
  return $ Move (toEnum (read hand :: Int) :: Hand) (read guess :: Integer)

round :: Morra
round = do
  pMove@(Move pHand pGuess) <- liftIO playerInput
  let oMove@(Move oHand oGuess) = Move Two 6
      moves = (pMove, oMove)
  state <- get
  put $ nextState state moves
  return moves
  where
    nextState gs@(GameState scores) ((Move pHand pGuess), (Move oHand oGuess))
      | sum == pGuess = mkState P1
      | sum == oGuess = mkState AI
      | otherwise = gs
      where
        sum = toInteger . getSum $ foldMap (Sum . fromEnum) [pHand, oHand]
        mkState pt = GameState $ M.insert pt ((scores M.! pt) + 1) scores

runGame :: IO ()
runGame = eval initialGameState
  where initialGameState = GameState $ M.fromList [(AI, 0), (P1, 0)]
        eval gs@(GameState scores)
          | scores M.! AI == 3 || scores M.! P1 == 3 = return ()
          | otherwise = do
              (_, nextGs) <- runStateT round gs
              eval nextGs

morra :: IO ()
morra = do
  putStrLn "Morra"
  runGame
  return ()
