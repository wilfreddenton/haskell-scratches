{-# LANGUAGE InstanceSigs #-}

newtype State s a = State { runState :: s -> (a, s) }

instance Functor (State s) where
  fmap :: (a -> b) -> State s a -> State s b
  fmap f (State g) = State (\s -> let (a, s') = g s in (f a, s'))

instance Applicative (State s) where
  pure :: a -> State s a
  pure a = State $ \s -> (a, s)
  (<*>) :: State s (a -> b) -> State s a -> State s b
  (<*>) (State f) (State g) = State $ \s ->
    let (f', s') = f s
        (a, s'') = g s'
    in (f' a, s'')

instance Monad (State s) where
  return = pure
  (>>=) :: State s a -> (a -> State s b) -> State s b
  (>>=) (State f) g = State $ \s ->
    let (a, s') = f s
        State f' = g a
    in f' s'

get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ \s' -> ((), s)

exec :: State s a -> s -> s
exec (State sa) s = snd $ sa s

eval :: State s a -> s -> a
eval (State sa) = fst . sa

modify :: (s -> s) -> State s ()
modify f = State $ \s -> ((), f s)
