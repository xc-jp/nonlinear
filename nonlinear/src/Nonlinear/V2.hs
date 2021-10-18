{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Nonlinear.V2 where

import Control.Applicative
import Data.Data (Data, Typeable)
import Data.Distributive
import Data.Functor.Classes
import Data.Functor.Rep (Representable)
import GHC.Generics (Generic, Generic1)

data V2 a = V2 {v21 :: !a, v22 :: !a}
  deriving stock (Eq, Show, Bounded, Ord, Functor, Foldable, Traversable, Generic, Generic1, Data, Typeable)
  deriving anyclass (Representable)

instance Distributive V2 where
  distribute f = V2 (v21 <$> f) (v22 <$> f)

instance Applicative V2 where
  pure a = V2 a a
  V2 f1 f2 <*> V2 a1 a2 = V2 (f1 a1) (f2 a2)

instance Monad V2 where
  V2 a b >>= f = V2 a' b'
    where
      V2 a' _ = f a
      V2 _ b' = f b

instance Semigroup a => Semigroup (V2 a) where V2 a b <> V2 a' b' = V2 (a <> a') (b <> b')

instance Monoid a => Monoid (V2 a) where mempty = V2 mempty mempty

instance Num a => Num (V2 a) where
  (+) = liftA2 (+)
  {-# INLINE (+) #-}
  (-) = liftA2 (-)
  {-# INLINE (-) #-}
  (*) = liftA2 (*)
  {-# INLINE (*) #-}
  negate = fmap negate
  {-# INLINE negate #-}
  abs = fmap abs
  {-# INLINE abs #-}
  signum = fmap signum
  {-# INLINE signum #-}
  fromInteger = pure . fromInteger
  {-# INLINE fromInteger #-}

instance Fractional a => Fractional (V2 a) where
  recip = fmap recip
  {-# INLINE recip #-}
  (/) = liftA2 (/)
  {-# INLINE (/) #-}
  fromRational = pure . fromRational
  {-# INLINE fromRational #-}

instance Floating a => Floating (V2 a) where
  pi = pure pi
  {-# INLINE pi #-}
  exp = fmap exp
  {-# INLINE exp #-}
  sqrt = fmap sqrt
  {-# INLINE sqrt #-}
  log = fmap log
  {-# INLINE log #-}
  (**) = liftA2 (**)
  {-# INLINE (**) #-}
  logBase = liftA2 logBase
  {-# INLINE logBase #-}
  sin = fmap sin
  {-# INLINE sin #-}
  tan = fmap tan
  {-# INLINE tan #-}
  cos = fmap cos
  {-# INLINE cos #-}
  asin = fmap asin
  {-# INLINE asin #-}
  atan = fmap atan
  {-# INLINE atan #-}
  acos = fmap acos
  {-# INLINE acos #-}
  sinh = fmap sinh
  {-# INLINE sinh #-}
  tanh = fmap tanh
  {-# INLINE tanh #-}
  cosh = fmap cosh
  {-# INLINE cosh #-}
  asinh = fmap asinh
  {-# INLINE asinh #-}
  atanh = fmap atanh
  {-# INLINE atanh #-}
  acosh = fmap acosh
  {-# INLINE acosh #-}

instance Eq1 V2 where liftEq f (V2 a b) (V2 a' b') = f a a' && f b b'

instance Ord1 V2 where liftCompare f (V2 a b) (V2 a' b') = f a a' <> f b b'

instance Show1 V2 where liftShowsPrec f _ d (V2 a b) = showsBinaryWith f f "V2" d a b
