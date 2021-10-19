{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Nonlinear.V4 where

import Control.Applicative
import Data.Data (Data, Typeable)
import Data.Distributive
import Data.Functor.Classes
import Data.Functor.Rep (Representable)
import GHC.Generics (Generic, Generic1)
import Lens.Micro.Internal (Field1 (..), Field2 (..), Field3 (..), Field4 (..))
import Lens.Micro.TH

data V4 a = V4 {_v4x :: !a, _v4y :: !a, _v4z :: !a, _v4w :: !a}
  deriving stock (Eq, Show, Bounded, Ord, Functor, Foldable, Traversable, Generic, Generic1, Data, Typeable)
  -- TODO Maybe use `linear`'s approach of using the lens as the `Rep`?'
  deriving anyclass (Representable)

instance Distributive V4 where
  distribute f = V4 (_v4x <$> f) (_v4y <$> f) (_v4z <$> f) (_v4w <$> f)

makeLenses ''V4

instance Field1 (V4 a) (V4 a) a a where _1 = v4x

instance Field2 (V4 a) (V4 a) a a where _2 = v4y

instance Field3 (V4 a) (V4 a) a a where _3 = v4z

instance Field4 (V4 a) (V4 a) a a where _4 = v4w

instance Applicative V4 where
  pure a = V4 a a a a
  V4 fx fy fz fw <*> V4 x y z w = V4 (fx x) (fy y) (fz z) (fw w)

instance Monad V4 where
  V4 x y z w >>= f = V4 (_v4x $ f x) (_v4y $ f y) (_v4z $ f z) (_v4w $ f w)

instance Semigroup x => Semigroup (V4 x) where V4 x y z w <> V4 x' y' z' w' = V4 (x <> x') (y <> y') (z <> z') (w <> w')

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

instance Eq1 V4 where liftEq f (V4 x y z w) (V4 x' y' z' w') = f x x' && f y y' && f z z' && f w w'

instance Ord1 V4 where liftCompare f (V4 x y z w) (V4 x' y' z' w') = f x x' <> f y y' <> f z z' <> f w w'

instance Show1 V4 where
  liftShowsPrec f _ d (V4 x y z w) =
    showParen (d > 10) $
      showString "V4 " . f 11 x . showChar ' ' . f 11 y . showChar ' ' . f 11 z . showChar ' ' . f 11 w
