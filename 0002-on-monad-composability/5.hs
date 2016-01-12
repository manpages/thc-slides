import XorFlip
import XorFlipT
import XorFlipMonadTrans

data Blam a = Blam
  deriving (Show)

newtype BlamT m a = BlamT {
  runBlamT :: m (Blam a)
}

fall :: (Monad m) => BlamT m a
fall = BlamT $ return Blam

instance (Monad m) => Functor (BlamT m) where
  _ `fmap` _ = fall

instance (Monad m) => Applicative (BlamT m) where
  pure _  = fall
  _ <*> _ = fall

instance (Monad m) => Monad (BlamT m) where
  return  = pure
  _ >>= _ = fall
