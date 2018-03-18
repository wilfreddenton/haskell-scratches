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

opponentMove :: PlayerType -> StdGen -> IO (Fingers, StdGen)
opponentMove AI g = do
  (g', fs) <- aiMove g
  putStrLn $ pack (show AI <> ": " <> (show $ fromEnum fs))
  return (fs, g')
opponentMove P2 g = do
  fs <- humanMove P2
  return (fs, g)

morra :: PlayerType -> Morra
morra t = do
  (GameState g) <- get
  pFingers <- liftIO $ humanMove P1
  (oFingers, g') <- liftIO $ opponentMove t g
  let hands = (pFingers, oFingers)
  put $ GameState g'
  liftIO $ putStrLn $ pack ("- " <> (show $ winner t hands) <> " wins")
  return hands
  where
    winner t (pFingers, oFingers)
      | even total = t
      | otherwise = P1
      where
        total = getSum $ foldMap (Sum . fromEnum) [pFingers, oFingers]

main :: IO ()
main = do
  putStrLn "-- Morra"
  putStr "Multiplayer? (y/n): "
  r <- getLine
  let t = if toLower r == "y" then P2 else AI
  putStrLn $ pack ("-- P1 is odds, " <> show t <> " is evens.")
  g <- newStdGen
  eval t $ GameState g
  where eval t s = do
          (_, nextGs) <- runStateT (morra t) s
          eval t nextGs
