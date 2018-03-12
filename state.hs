{-# LANGUAGE InstanceSigs #-}

import           Control.Applicative       (liftA3)
import           Control.Monad             (replicateM)
import           Control.Monad.Trans.State
import           System.Random

data Die = DieOne
         | DieTwo
         | DieThree
         | DieFour
         | DieFive
         | DieSix
         deriving (Eq, Show)

intToDie :: Int -> Die
intToDie n =
  case n of
    1 -> DieOne
    2 -> DieTwo
    3 -> DieThree
    4 -> DieFour
    5 -> DieFive
    _ -> DieSix

rollDie :: State StdGen Die
rollDie = state $ do
  (n, s) <- randomR (1, 6)
  return (intToDie n, s)

rollDie' :: State StdGen Die
rollDie' = intToDie <$> state (randomR (1, 6))

rollDieThreeTimes :: State StdGen (Die, Die, Die)
rollDieThreeTimes = liftA3 (,,) rollDie rollDie rollDie

nDie :: Int -> State StdGen [Die]
nDie n = replicateM n rollDie

rollsToGetTwenty :: StdGen -> Int
rollsToGetTwenty g = go 0 0 g
  where go :: Int -> Int -> StdGen -> Int
        go sum count gen
          | sum >= 20 = count
          | otherwise =
              let (die, nextGen) = randomR (1, 6) gen in go (sum + die) (count + 1) nextGen

rollsToGetN :: Int -> StdGen -> Int
rollsToGetN n g = go 0 0 n g
  where go :: Int -> Int -> Int -> StdGen -> Int
        go sum count limit gen
          | sum >= limit = count
          | otherwise =
              let (die, nextGen) = randomR (1, 6) gen in go (sum + die) (count + 1) limit nextGen

rollsCount :: Int -> StdGen -> (Int, [Die])
rollsCount n g = go 0 0 n [] g
  where go :: Int -> Int -> Int -> [Die] -> StdGen -> (Int, [Die])
        go sum count limit dice gen
          | sum >= limit = (count, dice)
          | otherwise =
              let (die, nextGen) = randomR (1, 6) gen
              in go (sum + die) (count + 1) limit (dice ++ [intToDie die]) nextGen


newtype Moi s a = Moi { runMoi :: s -> (a, s) }

instance Functor (Moi s) where
  fmap :: (a -> b) -> Moi s a -> Moi s b
  fmap f (Moi g) = Moi (\s -> let (a, s') = g s in (f a, s'))

instance Applicative (Moi s) where
  pure :: a -> Moi s a
  pure a = Moi $ \s -> (a, s)
  (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
  (<*>) (Moi f) (Moi g) = Moi $ \s ->
    let (f', s') = f s
        (a, s'') = g s'
    in (f' a, s'')

instance Monad (Moi s) where
  return = pure
  (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
  (>>=) (Moi f) g = Moi $ \s ->
    let (a, s') = f s
        Moi f' = g a
    in f' s'
