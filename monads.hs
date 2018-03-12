import Control.Monad (join)
import Control.Applicative

bind :: Monad m => (a -> m b) -> m a -> m b
bind f = join . fmap f


twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
  x <- xs
  if even x
    then [x*x, x*x]
    else [x*x]


data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First a) = First a
  fmap f (Second a) = Second $ f a

instance Applicative (Sum a) where
  pure = Second
  (<*>) (First a) _ = (First a)
  (<*>) (Second f) a = fmap f a

instance Monad (Sum a) where
  return = pure
  (>>=) (First a) _ = (First a)
  (>>=) (Second a) k = k a


data CountMe a = CountMe Integer a deriving (Eq, Show)

instance Functor CountMe where
  fmap f (CountMe i a) = CountMe (i + 1) (f a)

instance Applicative CountMe where
  pure = CountMe 0
  CountMe n f <*> CountMe n' a = CountMe (n + n') (f a)

instance Monad CountMe where
  return = pure
  CountMe n a >>= f =
    let CountMe _ b = f a in CountMe (n + 1) b


data Nope a = NopeDotJpg

instance Functor Nope where
  fmap _ NopeDotJpg = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  (<*>) _ _ = NopeDotJpg

instance Monad Nope where
  return = pure
  (>>=) _ _ = NopeDotJpg


newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
  pure = Identity
  (<*>) (Identity f) i = fmap f i

instance Monad Identity where
  return = pure
  (>>=) (Identity a) f = f a


data List a = Nil | Cons a (List a)

instance Monoid (List a) where
  mempty = Nil
  mappend Nil xs = xs
  mappend (Cons x xs) ys = Cons x $ mappend xs ys

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) $ fmap f xs

instance Applicative List where
  pure a = Cons a Nil
  (<*>) Nil _ = Nil
  (<*>) _ Nil = Nil
  (<*>) (Cons f fs) xs = mappend (fmap f xs) (fs <*> xs)

instance Monad List where
  return = pure
  (>>=) Nil _ = Nil
  (>>=) xs f = join $ fmap f xs


j :: Monad m => m (m a) -> m a
j = join

l1 :: Monad m => (a -> b) -> m a -> m b
l1 f ma = fmap f ma

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f ma mb = liftA2 f ma mb

a :: Monad m => m a -> m (a -> b) -> m b
a ma mf = mf <*> ma

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh (a:as) f = do
  b <- f a
  bs <- meh as f
  return (b:bs)

flipType :: (Monad m) => [m a] -> m [a]
flipType = flip meh id
