{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Adapted from [Linear.V1](https://hackage.haskell.org/package/linear-1.21.8/docs/Linear-V1.html)
module Nonlinear.V1 where

import Data.Data (Data, Typeable)
import Data.Functor.Classes
import Data.Functor.Identity
import Foreign (Storable)
import GHC.Generics (Generic, Generic1)
import GHC.Ix (Ix)
import Nonlinear.Internal (Lens')
import Nonlinear.Vector (Vec (..))

newtype V1 a = V1 {v1x :: a}
  deriving stock (Eq, Show, Read, Functor, Foldable, Traversable, Generic, Generic1, Data, Typeable)
  deriving newtype (Storable, Bounded, Ord, Num, Fractional, Floating, Semigroup, Monoid, Ix)
  deriving (Eq1, Read1, Show1, Ord1, Applicative, Monad) via Identity

instance Vec V1 where
  construct f = V1 (f _x)

class R1 t where
  _x :: Lens' (t a) a

instance R1 V1 where
  {-# INLINE _x #-}
  _x f (V1 a) = V1 <$> f a
