data XorFlip a = XorFlip Bool a

instance Functor XorFlip where
  f `fmap` XorFlip b x = XorFlip b $ f x

instance Applicative XorFlip where
  pure = XorFlip False
  XorFlip bx f <*> XorFlip by a = XorFlip bz $ f a
    where
      bz        = bx `xor` by
      a `xor` b = (a || b) && (not (a && b))

instance Monad XorFlip where
  return = XorFlip False
  XorFlip False x >>= f = f x
  XorFlip True  x >>= f = XorFlip (not b) r
    where XorFlip b r = f x
