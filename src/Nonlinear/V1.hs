{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Nonlinear.V1 where

import Data.Data (Data, Typeable)
import Data.Functor.Classes
import Data.Functor.Identity
import Foreign (Storable)
import GHC.Generics (Generic, Generic1)
import Nonlinear.Distributive (Distributive (..))
import Nonlinear.Internal (Lens', view)
import Nonlinear.Representable

newtype V1 a = V1 {v1x :: a}
  deriving stock (Eq, Show, Functor, Foldable, Traversable, Generic, Generic1, Data, Typeable)
  deriving newtype (Storable, Bounded, Ord, Num, Fractional, Floating, Semigroup, Monoid)
  deriving (Eq1, Show1, Ord1, Applicative, Monad) via Identity

instance Distributive V1 where
  distribute f = V1 (fmap v1x f)
  {-# INLINE distribute #-}

instance Representable V1 where
  type Rep V1 = E V1
  tabulate f = V1 (f $ E _x)
  {-# INLINE tabulate #-}
  index xs (E l) = view l xs
  {-# INLINE index #-}

class R1 t where
  _x :: Lens' (t a) a

instance R1 V1 where
  {-# INLINE _x #-}
  _x f (V1 a) = V1 <$> f a
