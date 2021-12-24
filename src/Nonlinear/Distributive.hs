{-# LANGUAGE ScopedTypeVariables #-}

-- | Adapted from [Data.Distributive](https://hackage.haskell.org/package/distributive-0.6.2.1/docs/Data-Distributive.html)
module Nonlinear.Distributive where

import Control.Applicative (WrappedMonad (WrapMonad), unwrapMonad)
import Data.Coerce (coerce)
import Data.Data (Proxy)
import Data.Functor.Identity (Identity (..))
import Data.Proxy (Proxy (Proxy))

class Functor g => Distributive g where
  {-# MINIMAL distribute | collect #-}
  distribute :: Functor f => f (g a) -> g (f a)
  distribute = collect id

  collect :: Functor f => (a -> g b) -> f a -> g (f b)
  collect f = distribute . fmap f

  distributeM :: Monad m => m (g a) -> g (m a)
  distributeM = fmap unwrapMonad . distribute . WrapMonad

  collectM :: Monad m => (a -> g b) -> m a -> g (m b)
  collectM f = distributeM . fmap f

cotraverse :: (Distributive g, Functor f) => (f a -> b) -> f (g a) -> g b
cotraverse f = fmap f . distribute

comapM :: (Distributive g, Monad m) => (m a -> b) -> m (g a) -> g b
comapM f = fmap f . distributeM

instance Distributive Identity where
  collect =
    coerce (fmap :: (a -> b) -> f a -> f b) ::
      forall a b f. Functor f => (a -> Identity b) -> f a -> Identity (f b)
  distribute = Identity . fmap runIdentity

instance Distributive Proxy where
  collect _ _ = Proxy
  distribute _ = Proxy
