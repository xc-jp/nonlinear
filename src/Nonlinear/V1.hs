{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Nonlinear.V1 where

import Data.Data (Data, Typeable)
import Data.Distributive
import Data.Functor.Classes
import Data.Functor.Identity
import Data.Functor.Rep (Representable)
import Foreign (Storable)
import GHC.Generics (Generic, Generic1)
import Lens.Micro.Internal (Field1 (..))
import Lens.Micro.TH

newtype V1 a = V1 {_v1x :: a}
  deriving stock (Eq, Show, Functor, Foldable, Traversable, Generic, Generic1, Data, Typeable)
  deriving newtype (Storable, Bounded, Ord, Num, Fractional, Floating, Semigroup, Monoid)
  deriving anyclass (Representable)
  deriving (Eq1, Show1, Ord1, Applicative, Monad) via Identity

instance Distributive V1 where distribute = V1 . fmap _v1x

makeLenses ''V1

instance Field1 (V1 a) (V1 a) a a where _1 = v1x
