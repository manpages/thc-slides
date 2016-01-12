import XorFlip
import XorFlipT
{--
newtype XorFlipT m a = XorFlipT {
  runXorFlipT :: m (XorFlip a)
}
--}

instance (Monad m) => Monad (XorFlipT m) where
  return x = XorFlipT $ return $ XorFlip False x
  m >>= f = XorFlipT $ do
    XorFlip bx a <- runXorFlipT m
    case bx of
      False -> runXorFlipT $ f a
      True  -> (g . f) a
    where
      g m = do
        XorFlip bx z <- runXorFlipT m
        return $ XorFlip (not bx) z
