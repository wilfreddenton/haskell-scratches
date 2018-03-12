module Composition where

import           Control.Applicative

newtype Identity a = Identity { runIdentity :: a }

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
  pure = Identity
  (<*>) (Identity f) (Identity a) = Identity $ f a


newtype Compose f g a = Compose { getCompose :: f (g a) } deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose fga) = Compose $ fmap (fmap f) fga

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure = Compose . pure .pure
  (<*>) (Compose fgf) (Compose fga) = Compose $ liftA2 (<*>) fgf fga

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
  foldMap f (Compose fga) = (foldMap . foldMap) f fga

instance (Traversable f, Traversable g) => Traversable (Compose f g) where
  traverse f (Compose fga) = Compose <$> (traverse . traverse) f fga


-- fmap (+1) $ Compose [Just 1, Nothing]
-- Compose {getCompose = [Just 2, Nothing]}

newtype One f a = One (f a) deriving (Eq, Show)
instance Functor f => Functor (One f) where
  fmap f (One fa) = One $ fmap f fa

newtype Three f g h a = Three (f (g (h a))) deriving (Eq, Show)
instance (Functor f, Functor g, Functor h) => Functor (Three f g h) where
  fmap f (Three fgha) = Three $ (fmap . fmap . fmap) f fgha


class Bifunctor p where
  {-# MINIMAL bimap | first, second #-}
  bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
  bimap f g = first f . second g

  first :: (a -> b) -> p a c -> p b c
  first f = bimap f id

  second :: (b -> c) -> p a b -> p a c
  second = bimap id


data Deux a b = Deux a b deriving (Show)

instance Bifunctor Deux where
  bimap f g (Deux a b) = Deux (f a) (g b)


data Const' a b = Const' a deriving (Show)

instance Bifunctor Const' where
  bimap f _ (Const' a) = Const' $ f a


data Drei a b c = Drei a b c deriving (Show)

instance Bifunctor (Drei a) where
  bimap f g (Drei a b c) = Drei a (f b) (g c)


data SuperDrei a b c = SuperDrei a b deriving (Show)

instance Bifunctor (SuperDrei a) where
  bimap f _ (SuperDrei a b) = SuperDrei a (f b)


data SemiDrei a b c = SemiDrei a deriving (Show)

instance Bifunctor (SemiDrei a) where
  bimap _ _ (SemiDrei a) = SemiDrei a


data Quadriceps a b c d = Quadzzz a b c d deriving (Show)

instance Bifunctor (Quadriceps a b) where
  bimap f g (Quadzzz a b c d) = Quadzzz a b (f c) (g d)


data Either' a b = Left' a | Right' b
instance Bifunctor Either' where
  bimap f _ (Left' a) = Left' $ f a
  bimap _ g (Right' b) = Right' $ g b


newtype IdentityT f a = IdentityT { runIdentityT :: f a } deriving (Eq, Show)

instance (Functor m) => Functor (IdentityT m) where
  fmap f (IdentityT fa) = IdentityT $ fmap f fa

instance (Applicative m) => Applicative (IdentityT m) where
  pure x = IdentityT $ pure x
  (<*>) (IdentityT ff) (IdentityT fa) = IdentityT $ ff <*> fa

instance (Monad m) => Monad (IdentityT m) where
  return = pure
  (>>=) (IdentityT ma) f = IdentityT $ ma >>= runIdentityT . f
