import Control.Applicative (liftA2)

data EitherIO e a = EitherIO {
    runEitherIO :: IO (Either e a)
}

instance Functor (EitherIO e) where
  fmap f = EitherIO . fmap (fmap f) . runEitherIO

instance Applicative (EitherIO e) where
  pure = EitherIO . return . Right
  (<*>) (EitherIO f) (EitherIO a) = EitherIO $ liftA2 (<*>) f a

instance Monad (EitherIO e) where
  return = pure
  (>>=) (EitherIO a) f = EitherIO $ a >>= either (return . Left) (runEitherIO .  f)
