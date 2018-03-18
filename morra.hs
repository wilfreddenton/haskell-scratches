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

module Morra where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.State
import qualified Data.Map                  as M
import           Data.Monoid
import           Prelude                   hiding (round)
import           System.Random

data Hand = Zero | One | Two | Three | Four | Five deriving (Enum, Show)
type Guess = Integer
data Move = Move Hand Guess
type Moves = (Move, Move)

data PlayerType = AI | P1 | P2 deriving (Eq, Ord, Show)
data GameState = GameState { gScores :: M.Map PlayerType Integer
                           , gSeed   :: StdGen }
type Morra = StateT GameState IO Moves

humanMove :: IO Move
humanMove = do
  putStr "\n0..5: "
  hand <- getLine
  putStr "sum: "
  guess <- getLine
  return $ Move (toEnum (read hand :: Int) :: Hand) (read guess :: Integer)

aiMove :: StdGen -> IO (StdGen, Move)
aiMove g = do
  let (guess, g') = randomR (0, 10) g
      (i, g'') = randomR (0, 5) g'
      hand = toEnum i :: Hand
  putStrLn $ show g''
  return $ (g'', Move hand guess)

opponentMove :: PlayerType -> GameState -> IO (Move, GameState)
opponentMove AI (GameState scores g) = do
  (g', move) <- aiMove g
  return (move, GameState scores g')
opponentMove P2 s = do
  move <- humanMove
  return (move, s)

round :: PlayerType -> Morra
round t = do
  s@(GameState scores g) <- get
  pMove <- liftIO $ humanMove
  (oMove, s') <- liftIO $ opponentMove t s
  let moves = (pMove, oMove)
  put $ nextState s' moves
  return moves
  where
    nextState s@(GameState scores g) ((Move pHand pGuess), (Move oHand oGuess))
      | sum == pGuess = mkState P1
      | sum == oGuess = mkState t
      | otherwise = s
      where
        sum = toInteger . getSum $ foldMap (Sum . fromEnum) [pHand, oHand]
        mkState pt = GameState (M.insert pt ((scores M.! pt) + 1) scores) g

runGame :: PlayerType -> IO ()
runGame t = do
  g <- newStdGen
  eval $ GameState (M.fromList [(P1, 0), (t, 0)]) g
  where eval gs@(GameState scores _)
          | scores M.! P1 == 3 || scores M.! t == 3 = return ()
          | otherwise = do
              (_, nextGs) <- runStateT (round t) gs
              eval nextGs

morra :: IO ()
morra = do
  putStrLn "Morra"
  runGame AI
  return ()
