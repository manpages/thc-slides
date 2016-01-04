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
type LensF' s a = forall f . (Functor f) => (a -> f a) -> s -> f s
ixf' :: Int -> LensF' [a] a
ixf' i f xs =
  iGuard i f xs ixDo
  where
    ixDo _ _ []       = lrgErr
    ixDo 0 f (x:rest) = (:rest) <$> f x
    ixDo i f (x:rest) = (x:) <$> ixDo (i-1) f rest
------------------------------------------------------------------------
type Lens s t a b = forall f . (Functor f) => (a -> f b) -> s -> f t
type Lens' s a = Lens s s a a
ix :: Int -> Lens' [a] a
ix i f []        = lrgErr
ix 0 f (x:rest)  = (:rest) <$> f x
ix i f (x:rest)
  | i < 0 = geqErr
  | True  = (x:) <$> ix (i-1) f rest
------------------------------------------------------------------------
over :: Lens s t a b -> (a -> b) -> s -> t
over l f = runIdentity . l (Identity . f)
------------------------------------------------------------------------
view :: Lens s t a b -> s -> a
view l = getConst . l Const
