{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

-- | Adapted from [Data.Functor.Contravariant.Rep](https://hackage.haskell.org/package/adjunctions-4.4/docs/Data-Functor-Contravariant-Rep.html#t:Representable)
module Nonlinear.Representable where

import Lens.Micro.Type (Lens')
import Nonlinear.Distributive

class Distributive f => Representable f where
  type Rep f :: *
  index :: f a -> Rep f -> a
  tabulate :: (Rep f -> a) -> f a

newtype E t = E (forall a. Lens' (t a) a)
