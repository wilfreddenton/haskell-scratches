{-# LANGUAGE TupleSections #-}

import           Control.Applicative
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Reader hiding (ReaderT (..))

import           Control.Monad

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance Functor m => Functor (MaybeT m) where
  fmap f (MaybeT ma) = MaybeT $ (fmap . fmap) f ma

instance Applicative m => Applicative (MaybeT m) where
  pure = MaybeT . pure . pure
  (<*>) (MaybeT mf) (MaybeT ma) = MaybeT $ liftA2 (<*>) mf ma

instance Monad m => Monad (MaybeT m) where
  return = pure
  (>>=) (MaybeT ma) f = MaybeT $ do
    v <- ma
    case v of
      Nothing -> return Nothing
      Just y  -> runMaybeT $ f y


newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }

instance Functor m => Functor (EitherT e m) where
  fmap f (EitherT mab) = EitherT $ (fmap . fmap) f mab

instance Applicative m => Applicative (EitherT e m) where
  pure = EitherT . pure . pure
  (<*>) (EitherT maf) (EitherT mab) = EitherT $ liftA2 (<*>) maf mab

instance Monad m => Monad (EitherT e m) where
  return = pure
  (>>=) (EitherT mab) f = EitherT $ do
    v <- mab
    case v of
      Left a  -> return $ Left a
      Right b -> runEitherT $ f b


swapEither :: Either e a -> Either a e
swapEither (Left e)  = Right e
swapEither (Right a) = Left a

swapEitherT :: Functor m => EitherT e m a -> EitherT a m e
swapEitherT (EitherT mea) = EitherT $ swapEither <$> mea

eitherT :: Monad m => (e -> m c) -> (a -> m c) -> EitherT e m a -> m c
eitherT f g (EitherT mab) = mab >>= either f g


newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

instance Functor m => Functor (ReaderT r m) where
  fmap f (ReaderT rma) = ReaderT $ (fmap . fmap) f rma

instance Applicative m => Applicative (ReaderT r m) where
  pure = ReaderT . pure .pure
  (<*>) (ReaderT rmf) (ReaderT rma) = ReaderT $ liftA2 (<*>) rmf rma

instance Monad m => Monad (ReaderT r m) where
  return = pure
  (>>=) (ReaderT rma) f = ReaderT $ \r -> do
    a <- rma r
    runReaderT (f a) r


newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

instance (Functor m) => Functor (StateT s m) where
  fmap f (StateT sma) = StateT $ \s -> helper <$> sma s
    where helper (a, s') = (f a, s')

instance (Monad m) => Applicative (StateT s m) where
  pure a = StateT $ \s -> pure (a, s)
  (<*>) (StateT smf) (StateT sma) = StateT $ \s -> do
    (f, s') <- smf s
    (a, s'') <- sma s'
    return (f a, s'')

instance (Monad m) => Monad (StateT s m) where
  return = pure
  (>>=) (StateT sma) f = StateT $ \s -> do
    (a, s') <- sma s
    runStateT (f a) s'


embedded :: MaybeT (ExceptT String (ReaderT () IO)) Int
embedded = MaybeT $ ExceptT $ ReaderT $ const $ pure $ Right $ Just 1


newtype IdentityT f a = IdentityT { runIdentityT :: f a }

instance MonadTrans IdentityT where
  lift = IdentityT

instance MonadTrans MaybeT where
  lift = MaybeT . fmap Just

instance MonadTrans (EitherT e) where
  lift = EitherT . fmap Right

instance MonadTrans (ReaderT r) where
  lift = ReaderT . const

instance MonadTrans (StateT s) where
  lift ma = StateT $ \s -> fmap (,s) ma


instance MonadIO m => MonadIO (MaybeT m) where
  liftIO = lift . liftIO

instance MonadIO m => MonadIO (ReaderT r m) where
  liftIO = lift . liftIO

instance MonadIO m => MonadIO (StateT s m) where
  liftIO = lift . liftIO
