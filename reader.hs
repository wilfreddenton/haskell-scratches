{-# LANGUAGE InstanceSigs #-}

import           Control.Applicative
import           Data.Char
import           Data.Maybe
import           Data.Monoid

boop = (*2)
doop = (+10)

bip :: Integer -> Integer
bip = boop . doop

bloop :: Integer -> Integer
bloop = fmap boop doop

bbop :: Integer -> Integer
bbop = (+) <$> boop <*> doop

duwop :: Integer -> Integer
duwop = liftA2 (+) boop doop

cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

composed :: [Char] -> [Char]
composed = cap . rev

fmapped :: [Char] -> [Char]
fmapped = fmap cap rev

tupled :: [Char] -> ([Char], [Char])
tupled = (,) <$> cap <*> rev

tupled' :: [Char] -> ([Char], [Char])
tupled' = do
  a <- rev
  b <- cap
  return (a, b)

tupled'' :: [Char] -> ([Char], [Char])
tupled'' = rev >>= \a -> cap >>= \b -> return (a,b)


newtype Reader r a = Reader { runReader :: r -> a }

ask :: Reader a a
ask = Reader id


myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 f fa fb = f <$> fa <*> fb


asks :: (r -> a) -> Reader r a
asks f = Reader f

instance Functor (Reader r) where
  fmap f (Reader ra) = Reader $ f . ra

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure a = Reader $ const a
  (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
  (<*>) (Reader rab) (Reader ra) =
    Reader $ \r -> rab r $ ra r

instance Monad (Reader r) where
  return = pure
  (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  (>>=) (Reader ra) aRb =
    Reader $ \r -> (runReader . aRb $ ra r) r


x = [1, 2, 3]
y = [4, 5, 6]
z = [7, 8, 9]

xs :: Maybe Integer
xs = lookup 3 $ zip x y

ys :: Maybe Integer
ys = lookup 6 $ zip y z

zs :: Maybe Integer
zs = lookup 4 $ zip x y

z' :: Integer -> Maybe Integer
z' = flip lookup $ zip x z

x1 :: Maybe (Integer, Integer)
x1 = liftA2 (,) xs ys

x2 :: Maybe (Integer, Integer)
x2 = liftA2 (,) ys zs

x3 :: Integer -> (Maybe Integer, Maybe Integer)
x3 = z' >>= \z'' -> return (z'', z'')

summed :: Num c => (c, c) -> c
summed = uncurry (+)

bolt :: Integer -> Bool
bolt = liftA2 (&&) (>3) (<8)

sequA :: Integral a => a -> [Bool]
sequA m = sequenceA [(>3), (<8), even] m

s' = summed <$> ((,) <$> xs <*> ys)

main :: IO ()
main = do
  print $ sequenceA [Just 3, Just 2, Just 1]
  print $ sequenceA [x, y]
  print $ sequenceA [xs, ys]
  print $ summed <$> ((,) <$> xs <*> ys)
  print $ fmap summed ((,) <$> xs <*> zs)
  print $ bolt 7
  print $ fmap bolt z
  print $ sequenceA [(>3), (<8), even] 7
  print $ getAll . foldMap All $ sequA 6
  print $ sequA $ fromMaybe 6 s'
  print $ bolt $ fromMaybe 6 ys
