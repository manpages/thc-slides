module Main where
import Util

type Lens s a = (a -> [a]) -> s -> (a, [s])

ix :: Int -> Lens [a] a
ix i f xs =
  iGuard i f xs ixDo
  where
    ixDo _ _ []       = lrgErr
    ixDo 0 f (x:rest) = (x, [(x':rest) | x' <- f x])
    ixDo i f (x:rest) = let (x', rest') = ixDo (i-1) f rest in
                            (x', map (x:) rest')
