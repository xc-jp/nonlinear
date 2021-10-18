{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Nonlinear.V4 where

import Control.Applicative
import Data.Data (Data, Typeable)
import Data.Distributive
import Data.Functor.Classes
import Data.Functor.Rep (Representable)
import GHC.Generics (Generic, Generic1)

data V4 a = V4 {v41 :: !a, v42 :: !a, v43 :: !a, v44 :: !a}
  deriving stock (Eq, Show, Bounded, Ord, Functor, Foldable, Traversable, Generic, Generic1, Data, Typeable)
  deriving anyclass (Representable)

instance Distributive V4 where
  distribute f = V4 (v41 <$> f) (v42 <$> f) (v43 <$> f) (v44 <$> f)

instance Applicative V4 where
  pure a = V4 a a a a
  V4 f1 f2 f3 f4 <*> V4 a1 a2 a3 a4 = V4 (f1 a1) (f2 a2) (f3 a3) (f4 a4)

instance Monad V4 where
  V4 a b c d >>= f = V4 a' b' c' d'
    where
      V4 a' _ _ _ = f a
      V4 _ b' _ _ = f b
      V4 _ _ c' _ = f c
      V4 _ _ _ d' = f d

instance Semigroup a => Semigroup (V4 a) where V4 a b c d <> V4 a' b' c' d' = V4 (a <> a') (b <> b') (c <> c') (d <> d')

instance Monoid a => Monoid (V4 a) where mempty = V4 mempty mempty mempty mempty

instance Num a => Num (V4 a) where
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

instance Fractional a => Fractional (V4 a) where
  recip = fmap recip
  {-# INLINE recip #-}
  (/) = liftA2 (/)
  {-# INLINE (/) #-}
  fromRational = pure . fromRational
  {-# INLINE fromRational #-}

instance Floating a => Floating (V4 a) where
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

instance Eq1 V4 where liftEq f (V4 a b c d) (V4 a' b' c' d') = f a a' && f b b' && f c c' && f d d'

instance Ord1 V4 where liftCompare f (V4 a b c d) (V4 a' b' c' d') = f a a' <> f b b' <> f c c' <> f d d'

instance Show1 V4 where
  liftShowsPrec f _ z (V4 a b c d) =
    showParen (z > 10) $
      showString "V4 " . f 11 a . showChar ' ' . f 11 b . showChar ' ' . f 11 c . showChar ' ' . f 11 d
