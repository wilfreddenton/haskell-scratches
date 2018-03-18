{-# LANGUAGE OverloadedStrings #-}
module Morra where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.State
import qualified Data.Map                  as M
import           Data.Monoid
import           Data.Text
import           Data.Text.IO
import           Prelude                   hiding (getLine, putStr, putStrLn,
                                            round)
import           System.Random

data Fingers = One | Two deriving (Bounded, Eq, Ord, Show)

instance Enum Fingers where
  toEnum 1 = One
  toEnum _ = Two
  fromEnum One = 1
  fromEnum Two = 2

type Hands = (Fingers, Fingers)

data PlayerType = AI | P1 | P2 deriving (Eq, Ord, Show)

data GameState = GameState { gSeed :: StdGen }

type Morra = StateT GameState IO Hands

humanMove :: PlayerType -> IO Fingers
humanMove t = do
  putStr $ pack (show t <> ": ")
  l <- getLine
  return (toEnum (read $ unpack l :: Int) :: Fingers)

aiMove :: StdGen -> IO (StdGen, Fingers)
aiMove g = do
  let (i, g') = randomR (1, 2) g
  return $ (g', toEnum i :: Fingers)

opponentMove :: PlayerType -> GameState -> IO (Fingers, GameState)
opponentMove AI (GameState g) = do
  (g', fs) <- aiMove g
  putStrLn $ pack (show AI <> ": " <> (show $ fromEnum fs))
  return (fs, GameState g')
opponentMove P2 s = do
  fs <- humanMove P2
  return (fs, s)

round :: PlayerType -> Morra
round t = do
  s@(GameState g) <- get
  pFingers <- liftIO $ humanMove P1
  (oFingers, s') <- liftIO $ opponentMove t s
  let hands = (pFingers, oFingers)
  put s'
  liftIO $ putStrLn $ pack ("- " <> (show $ winner t hands) <> " wins")
  return hands
  where
    winner t (pFingers, oFingers)
      | even sum = t
      | otherwise = P1
      where
        sum = toInteger . getSum $ foldMap (Sum . fromEnum) [pFingers, oFingers]

runGame :: PlayerType -> IO ()
runGame t = do
  g <- newStdGen
  eval $ GameState g
  where eval s = do
          (_, nextGs) <- runStateT (round t) s
          eval nextGs

morra :: IO ()
morra = do
  putStrLn "-- Morra"
  putStr "Multiplayer? (y/n): "
  r <- getLine
  let t = if toLower r == "y" then P2 else AI
  putStrLn $ pack ("-- P1 is odds, " <> show t <> " is evens.")
  runGame t
  return ()
