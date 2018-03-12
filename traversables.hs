import           Data.Monoid

data Query     = Query
data SomeObj   = SomeObj
data IoOnlyObj = IoOnlyObj
data Err       = Err

decodeFn :: String -> Either Err SomeObj
decodeFn = undefined

fetchFn :: Query -> IO [String]
fetchFn = undefined

makeIoOnlyObj :: [SomeObj]
              -> IO [(SomeObj, IoOnlyObj)]
makeIoOnlyObj = undefined

pipelineFn :: Query
           -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn = (traverse makeIoOnlyObj
              . traverse decodeFn =<<) . fetchFn


data Or a b = This a
            | That b
            deriving (Eq, Ord, Show)

instance Functor (Or a) where
  fmap _ (This a) = This a
  fmap f (That b) = That $ f b

instance Applicative (Or a) where
  pure = That
  (<*>) (That f) that = fmap f that
  (<*>) (This e) _    = This e

instance Foldable (Or a) where
  foldMap f (That a) = f a
  foldMap _ (This _) = mempty

instance Traversable (Or a) where
  traverse f (That a) = fmap That $ f a
  traverse _ (This x) = pure $ This x


data Two a b = Two a b

instance Functor (Two a) where
  fmap f (Two a b) = Two a $ f b

instance Monoid a => Applicative (Two a) where
  pure b = Two mempty b
  (<*>) (Two a f) (Two a' b) = Two (a <> a') $ f b

instance Foldable (Two a) where
  foldMap f (Two _ b) = f b

instance Traversable (Two a) where
  traverse f (Two a b) = fmap (Two a) $ f b


newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
  pure = Identity
  (<*>) (Identity f) (Identity a) = Identity $ f a

instance Foldable Identity where
  foldMap f (Identity a) = f a

instance Traversable Identity where
  traverse f (Identity a) = fmap Identity $ f a


newtype Constant a b = Constant { getConstant :: a }

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

instance Monoid a => Applicative (Constant a) where
  pure _ = Constant mempty
  (<*>) (Constant a) (Constant a') = Constant $ a <> a'

instance Foldable (Constant a) where
  foldMap _ (Constant _) = mempty

instance Traversable (Constant a) where
  traverse _ (Constant a) = pure $ Constant a


data Optional a = Nada | Yep a

instance Functor Optional where
  fmap f (Yep a) = Yep $ f a
  fmap _ Nada    = Nada

instance Applicative Optional where
  pure = Yep
  (<*>) (Yep f) ya = fmap f ya
  (<*>) Nada _     = Nada

instance Foldable Optional where
  foldMap f (Yep a) = f a
  foldMap _ Nada    = mempty

instance Traversable Optional where
  traverse f (Yep a) = Yep <$> f a
  traverse _ Nada    = pure Nada


data List a = Nil | Cons a (List a)

instance Functor List where
  fmap f (Cons a as) = Cons (f a) $ fmap f as
  fmap _ Nil         = Nil

instance Monoid (List a) where
  mempty = Nil
  mappend (Cons a as) bs = Cons a $ mappend as bs
  mappend Nil bs         = bs

instance Applicative List where
  pure a = Cons a Nil
  (<*>) (Cons f fs) as = mappend (fmap f as) $ fs <*> as
  (<*>) Nil _          = Nil

instance Foldable List where
  foldMap f (Cons a as) = mappend (f a) $ foldMap f as

instance Traversable List where
  traverse f (Cons a as) = Cons <$> (f a) <*> traverse f as
  traverse _ Nil         = pure Nil


data Three a b c = Three a b c

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b $ f c

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure c = Three mempty mempty c
  (<*>) (Three a b f) (Three a' b' c) = Three (a <> a') (b <> b') $ f c

instance Foldable (Three a b) where
  foldMap f (Three _ _ c) = f c

instance Traversable (Three a b) where
  traverse f (Three a b c) = (Three a b) <$> f c


data Three' a b = Three' a b b

instance Functor (Three' a) where
  fmap f (Three' a b b') = Three' a (f b) $ f b'

instance Monoid a => Applicative (Three' a) where
  pure b = Three' mempty b b
  (<*>) (Three' a f f') (Three' a' b c) = Three' (a <> a') (f b) $ f' c

instance Foldable (Three' a) where
  foldMap f (Three' a b b') = f b <> f b'

instance Traversable (Three' a) where
  traverse f (Three' a b b') = Three' a <$> f b <*> f b'


data S n a = S (n a) a

instance Functor n => Functor (S n) where
  fmap f (S n a) = S (fmap f n) $ f a

instance Foldable n => Foldable (S n) where
  foldMap f (S n a) = (foldMap f n) <> f a

instance Traversable n => Traversable (S n) where
  traverse f (S n a) = S <$> traverse f n <*> f a


data Tree a = Empty
            | Leaf a
            | Node (Tree a) a (Tree a)
            deriving (Eq, Show)

instance Functor Tree where
  fmap f Empty        = Empty
  fmap f (Leaf a)     = Leaf $ f a
  fmap f (Node l a r) = Node (fmap f l) (f a) (fmap f r)

instance Foldable Tree where
  foldMap f Empty        = mempty
  foldMap f (Leaf a)     = f a
  foldMap f (Node l a r) = foldMap f l <> f a <> foldMap f r

instance Traversable Tree where
  traverse _ Empty        = pure Empty
  traverse f (Leaf a)     = Leaf <$> f a
  traverse f (Node l a r) = Node <$> traverse f l <*> f a <*> traverse f r
