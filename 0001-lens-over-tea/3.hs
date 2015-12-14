module Main where
import Util
------------------------------------------------------------------------
type LensF s a = forall f . (Functor f) => (a -> f a) -> s -> (a, f s)
ixf :: Int -> LensF [a] a
ixf i f xs =
  iGuard i f xs ixDo
  where
    ixDo _ _ []       = lrgErr
    ixDo 0 f (x:rest) = (x, (:rest) <$> f x)
    ixDo i f (x:rest) = let (x', rest') = ixDo (i-1) f rest in
                            (x', (x:) <$> rest')
------------------------------------------------------------------------
type Lens s a = forall f . (Functor f) => (a -> f a) -> s -> f s
ix :: Int -> Lens [a] a
ix i f xs =
  iGuard i f xs ixDo
  where
    ixDo _ _ []       = lrgErr
    ixDo 0 f (x:rest) = (:rest) <$> f x
    ixDo i f (x:rest) = (x:) <$> ixDo (i-1) f rest
