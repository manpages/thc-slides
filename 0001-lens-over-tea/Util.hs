module Util ( ($>),       Compose (..), geqErr, lrgErr, iGuard,
              Const (..), Product (..), Identity (..),  first,
              second,     pamf,         bimap,          (>$<)
            ) where

import           Data.Bifunctor        (first, second, bimap)
import           Data.Functor          (($>))
import           Data.Functor.Compose  (Compose (..))
import           Data.Functor.Identity (runIdentity, Identity(..))
import           Data.Functor.Product  (Product (..))
import           Control.Applicative   (Const (..))

geqErr = error "index must be geq 0"
lrgErr = error "index too large"
iGuard x y z f =
  if x < 0
    then geqErr
    else f x y z

pamf  = flip fmap
(>$<) = pamf
