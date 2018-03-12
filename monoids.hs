import Data.Monoid

data Optional a = Nada | Only a deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
  mempty = Nada

  mappend Nada Nada = Nada
  mappend (Only b) Nada = Only b
  mappend Nada (Only a) = Only a
  mappend (Only a) (Only b) = Only $ mappend a b
