{-# LANGUAGE FlexibleInstances #-}

a = fmap (+1) $ read "[1]" :: [Int]

b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])

c = fmap (*2) (\x -> x - 2)

d = fmap ((return '1' ++) . show) (\x -> [x, 1..3])

e :: IO Integer
e = do
  let ioi = readIO "1" :: IO Integer
      changed = fmap (read . ("123"++) . show) ioi
  fmap (*3) changed


data Possibly a = LolNope | Yeppers a deriving (Eq, Show)

instance Functor Possibly where
  fmap _ LolNope = LolNope
  fmap f (Yeppers a) = Yeppers $ f a


data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First a) = First a
  fmap f (Second b) = Second $ f b


data Wrap f a = Wrap (f a) deriving (Eq, Show)

instance Functor f => Functor (Wrap f) where
  fmap f (Wrap fa) = Wrap $ fmap f fa


data Quant a b = Finance | Desk a | Bloor b

instance Functor (Quant a) where
  fmap _ Finance = Finance
  fmap _ (Desk a) = Desk a
  fmap f (Bloor b) = Bloor $ f b


data K a b = K a

instance Functor (K a) where
  fmap _ (K a) = K a


newtype Flip f a b = Flip (f b a) deriving (Eq, Show)

newtype K' a b = K' a

instance Functor (Flip K' a) where
  fmap f (Flip (K' a)) = Flip $ K' $ f a


data EvilGoateeConst a b = GoatyConst b

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst b) = GoatyConst $ f b


data LiftItOut f a = LiftItOut (f a)

instance Functor f => Functor (LiftItOut f) where
  fmap f (LiftItOut fa) = LiftItOut $ fmap f fa


data Parappa f g a = DaWrappa (f a) (g a)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap f (DaWrappa fa ga) = DaWrappa (fmap f fa) (fmap f ga)


data IgnoreOne f g a b = IgnoringSomething (f a) (g b)

instance (Functor g) => Functor (IgnoreOne f g a) where
  fmap f (IgnoringSomething fa fb) = IgnoringSomething fa $ fmap f fb


data Notorious g o a t = Notorious (g o) (g a) (g t)

instance (Functor g) => Functor (Notorious g o a) where
  fmap f (Notorious go ga gt) = Notorious go ga $ fmap f gt


data List a = Nil | Cons a (List a)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a la) = Cons (f a) $ fmap f la


data GoatLord a = NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)

instance Functor GoatLord where
  fmap _ NoGoat = NoGoat
  fmap f (OneGoat a) = OneGoat $ f a
  fmap f (MoreGoats ga1 ga2 ga3) = MoreGoats (fmap f ga1) (fmap f ga2) (fmap f ga3)

data TalkToMe a = Halt | Print String a | Read (String -> a)

instance Functor TalkToMe where
  fmap _ Halt = Halt
  fmap f (Print s a) = Print s $ f a
  fmap f (Read sa) = Read $ f . sa
