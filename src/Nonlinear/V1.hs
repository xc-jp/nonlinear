{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Nonlinear.V1 where

import Data.Data (Data, Typeable)
import Data.Functor.Classes
import Data.Functor.Identity
import Foreign (Storable)
import GHC.Generics (Generic, Generic1)
import Lens.Micro.Internal (Field1 (..))
import Lens.Micro.Type (Lens')
import Nonlinear.Distributive (Distributive (..))

newtype V1 a = V1 {v1x :: a}
  deriving stock (Eq, Show, Functor, Foldable, Traversable, Generic, Generic1, Data, Typeable)
  deriving newtype (Storable, Bounded, Ord, Num, Fractional, Floating, Semigroup, Monoid)
  deriving (Eq1, Show1, Ord1, Applicative, Monad) via Identity

instance Distributive V1 where
  distribute f = V1 (fmap v1x f)
  {-# INLINE distribute #-}

instance Field1 (V1 a) (V1 a) a a where
  {-# INLINE _1 #-}
  _1 f (V1 a) = V1 <$> f a

class R1 t where
  _x :: Lens' (t a) a

instance R1 V1 where
  {-# INLINE _x #-}
  _x = _1
