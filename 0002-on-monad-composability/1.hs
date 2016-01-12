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

unfize :: (Monad f, Monad g) => (f a -> Compose g f b) -> (a -> Compose g f b)
unfize phi = \a -> (phi (return a))

fize :: (Monad f, Monad g) => (a -> Compose g f b) -> (f a -> Compose g f b)
fize _ = error "This function doesn't exist because of lemma #1. We would need to have intermediate function of type ``f a -> a`` to call 1st argument function from the result."

instance (Monad f, Monad g) => Monad (Compose g f) where
  return x = Compose (return (return x))
  (Compose gfa) >>= phi = _

{--
instance (Monad n, Monad m) => Monad (Compose n m) where
  -- return :: a -> Compose n m a
  return x = Compose (return (return x))
  -- (>>=) :: (Compose n m a) -> (a -> Compose n m b) -> (Compose n m b)
  nma >>= a_nmb = 
--}
