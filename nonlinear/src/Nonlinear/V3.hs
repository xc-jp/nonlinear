{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Nonlinear.V3 where

import Control.Applicative
import Data.Data (Data, Typeable)
import Data.Distributive
import Data.Functor.Classes
import Data.Functor.Rep (Representable)
import GHC.Generics (Generic, Generic1)

data V3 a = V3 {v31 :: !a, v32 :: !a, v33 :: !a}
  deriving stock (Eq, Show, Bounded, Ord, Functor, Foldable, Traversable, Generic, Generic1, Data, Typeable)
  deriving anyclass (Representable)

instance Distributive V3 where
  distribute f = V3 (v31 <$> f) (v32 <$> f) (v33 <$> f)

instance Applicative V3 where
  pure a = V3 a a a
  V3 f1 f2 f3 <*> V3 a1 a2 a3 = V3 (f1 a1) (f2 a2) (f3 a3)

instance Monad V3 where
  V3 a b c >>= f = V3 a' b' c'
    where
      V3 a' _ _ = f a
      V3 _ b' _ = f b
      V3 _ _ c' = f c

instance Semigroup a => Semigroup (V3 a) where V3 a b c <> V3 a' b' c' = V3 (a <> a') (b <> b') (c <> c')

instance Monoid a => Monoid (V3 a) where mempty = V3 mempty mempty mempty

instance Num a => Num (V3 a) where
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

instance Fractional a => Fractional (V3 a) where
  recip = fmap recip
  {-# INLINE recip #-}
  (/) = liftA2 (/)
  {-# INLINE (/) #-}
  fromRational = pure . fromRational
  {-# INLINE fromRational #-}

instance Floating a => Floating (V3 a) where
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

instance Eq1 V3 where liftEq f (V3 a b c) (V3 a' b' c') = f a a' && f b b' && f c c'

instance Ord1 V3 where liftCompare f (V3 a b c) (V3 a' b' c') = f a a' <> f b b' <> f c c'

instance Show1 V3 where
  liftShowsPrec f _ d (V3 a b c) =
    showParen (d > 10) $
      showString "V3 " . f 11 a . showChar ' ' . f 11 b . showChar ' ' . f 11 c
