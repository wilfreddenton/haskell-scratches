import Control.Applicative
import Data.List (elemIndex)

added :: Maybe Integer
added = fmap (+3) (lookup 3 $ zip [1, 2, 3] [4, 5, 6])


y :: Maybe Integer
y = lookup 3 $ zip [1, 2, 3] [4, 5, 6]

z :: Maybe Integer
z = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

tupled :: Maybe (Integer, Integer)
tupled = liftA2 (,) y z


x :: Maybe Int
x = elemIndex 3 [1, 2, 3, 4, 5]

y' :: Maybe Int
y' = elemIndex 4 [1, 2, 3, 4, 5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = liftA2 max' x y'


xs = [1, 2, 3]
ys = [4, 5, 6]

x' :: Maybe Integer
x' = lookup 3 $ zip xs ys

y'' :: Maybe Integer
y'' = lookup 2 $ zip xs ys

summed :: Maybe Integer
summed = fmap sum $ liftA2 (,) x' y''


newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
  pure = Identity
  (<*>) (Identity f) (Identity a) = Identity $ f a


newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

instance Monoid a => Applicative (Constant a) where
  pure _ = Constant { getConstant = mempty }
  (<*>) (Constant a) (Constant a') = Constant $ mappend a a'


test = const <$> Just "Hello" <*> pure "World"

test1 = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> pure [1, 2, 3]


data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Monoid (List a) where
  mempty = Nil
  mappend Nil Nil = Nil
  mappend Nil ys = ys
  mappend (Cons x xs) ys = Cons x $ mappend xs ys

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) $ fmap f xs

instance Applicative List where
  pure a = Cons a Nil
  (<*>) Nil Nil = Nil
  (<*>) Nil _ = Nil
  (<*>) (Cons f fs) ys = mappend (f <$> ys) (fs <*> ys)


data Validation e a = Failure e | Success a deriving (Eq, Show)

instance Monoid e => Applicative (Validation e) where
  pure = Success
  (<*>) (Failure e) (Failure e') = Failure $ mappend e e'
  (<*>) (Failure e) (Success a) = Failure e
  (<*>) (Success a) (Failure e) = Failure e
  (<*>) (Success f) (Success a) = Success $ f a

instance Functor (Validation e) where
  fmap _ (Failure e) = Failure e
  fmap f (Success a) = Success $ f a


data Pair a = Pair a a deriving Show

instance Functor Pair where
  fmap f (Pair a b) = Pair (f a) (f b)

instance Applicative Pair where
  pure a = Pair a a
  (<*>) (Pair f f') (Pair a a') = Pair (f a) (f' a')


data Two a b = Two a b

instance Functor (Two a) where
  fmap f (Two a b) = Two a $ f b

instance Monoid a => Applicative (Two a) where
  pure = Two mempty
  (<*>) (Two a f) (Two a' b) = Two (mappend a a') (f b)


data Three a b c = Three a b c

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b $ f c

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure = Three mempty mempty
  (<*>) (Three a b f) (Three a' b' c) = Three (mappend a a') (mappend b b') (f c)


data Three' a b = Three' a b b

instance Functor (Three' a) where
  fmap f (Three' a b b') = Three' a (f b) (f b')

instance Monoid a => Applicative (Three' a) where
  pure b = Three' mempty b b
  (<*>) (Three' a f f') (Three' a' b c) = Three' (mappend a a') (f b) (f c)


data Four a b c d = Four a b c d

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
  pure = Four mempty mempty mempty
  (<*>) (Four a b c f) (Four a' b' c' d) = Four (mappend a a') (mappend b b') (mappend c c') (f d)


data Four' a b = Four' a a a b

instance Functor (Four' a) where
  fmap f (Four' a a' a'' b) = Four' a a' a'' (f b)

instance Monoid a => Applicative (Four' a) where
  pure = Four' mempty mempty mempty
  (<*>) (Four' a b c f) (Four' a' b' c' d) = Four' (mappend a a') (mappend b b') (mappend c c') (f d)
