module XorFlip where

data XorFlip a = XorFlip Bool a
  deriving (Show)

instance Functor XorFlip where
  f `fmap` XorFlip b x = XorFlip b $ f x

instance Applicative XorFlip where
  pure = XorFlip False
  XorFlip bx f <*> XorFlip by a = XorFlip bz $ f a
    where
      bz        = bx `xor` by

instance Monad XorFlip where
  return = XorFlip False
  XorFlip False x >>= f = f x
  XorFlip True  x >>= f = XorFlip (not b) r
    where XorFlip b r = f x

a `xor` b = (a || b) && (not (a && b))
