module XorFlipT where

import XorFlip

newtype XorFlipT m a = XorFlipT {
  runXorFlipT :: m (XorFlip a)
}

instance (Monad m) => Functor (XorFlipT m) where
  f `fmap` ma = XorFlipT $ do
    XorFlip b a <- runXorFlipT ma
    return $ XorFlip b $ f a

instance (Monad m) => Applicative (XorFlipT m) where
  pure x = XorFlipT $ return $ XorFlip False x
  mf <*> ma = XorFlipT $ do
    XorFlip bx f <- runXorFlipT mf
    XorFlip by a <- runXorFlipT ma
    return $ XorFlip (bx `xor` by) $ f a
