{--
-- Newtype below is exactly the same.
data Compose f g a = Compose (f (g a))
getCompose :: Compose f g a -> f (g a)
getCompose (Compose x) = x
--}

newtype Compose f g a = Compose { getCompose :: f (g a) }

instance (Functor f, Functor g) => Functor (Compose f g) where
  -- fmap :: (a -> b) -> Compose f g a -> Compose f g b
  fmap f (Compose x) = Compose (fmap (fmap f) x)

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure x = Compose (pure (pure x))
  -- Compose f g (a -> b) -> Compose f g a -> Compose f g b
  Compose f <*> Compose x = Compose ((<*>) <$> f <*> x)
