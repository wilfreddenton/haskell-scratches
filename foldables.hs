import           Data.Monoid

data Identity a = Identity a

instance Foldable Identity where
  foldr f b (Identity a) = f a b
  foldl f b (Identity a) = f b a
  foldMap f (Identity a) = f a


data Optional a = Nil | Only a

instance Foldable Optional where
  foldr _ b Nil      = b
  foldr f b (Only a) = f a b

  foldl _ b Nil      = b
  foldl f b (Only a) = f b a

  foldMap _ Nil      = mempty
  foldMap f (Only a) = f a

sum :: (Foldable t, Num a) => t a -> a
sum = getSum . foldMap Sum


product :: (Foldable t, Num a) => t a -> a
product = getProduct . foldMap Product


elem :: (Foldable t, Eq a) => a -> t a -> Bool
elem a = getAny . foldMap (\x -> Any $ x == a)


null :: (Foldable t) => t a -> Bool
null = getAll . foldMap (\_ -> All False)


len :: (Foldable t) => t a -> Int
len = getSum . foldMap (\_ -> Sum 1)


toArr :: (Foldable t) => t a -> [a]
toArr = foldr (:) []


reduce :: (Foldable t, Monoid m) => t m -> m
reduce = foldr (<>) mempty


reduceMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
reduceMap f = foldr (\a b -> f a <> b) mempty


data Constant a b = Constant a

instance Foldable (Constant a) where
  foldMap f (Constant a) = mempty


data Two a b = Two a b

instance Foldable (Two a) where
  foldMap f (Two a b) = f b


data Three a b c = Three a b c

instance Foldable (Three a b) where
  foldMap f (Three a b c) = f c


data Three' a b = Three' a b b

instance Foldable (Three' a) where
  foldMap f (Three' a b b') = f b <> f b'


data Four a b = Four a b b b

instance Foldable (Four a) where
  foldMap f (Four a b b' b'') = f b <> f b' <> f b''


filterF :: (Applicative f, Foldable t, Monoid (f a)) => (a -> Bool) -> t a -> f a
filterF f = foldMap (\x -> if f x then pure x else mempty)
