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

module Nonlinear.V2 where

import Control.Applicative
import Data.Data (Data, Typeable)
import Data.Distributive
import Data.Functor.Classes
import Data.Functor.Rep (Representable)
import GHC.Generics (Generic, Generic1)
import Lens.Micro.Internal (Field1 (..), Field2 (..))
import Lens.Micro.TH

data V2 a = V2 {_v2x :: !a, _v2y :: !a}
  deriving stock (Eq, Show, Bounded, Ord, Functor, Foldable, Traversable, Generic, Generic1, Data, Typeable)
  -- TODO Maybe use `linear`'s approach of using the lens as the `Rep`?'
  deriving anyclass (Representable)

instance Distributive V2 where
  distribute f = V2 (_v2x <$> f) (_v2y <$> f)

makeLenses ''V2

instance Field1 (V2 a) (V2 a) a a where _1 = v2x

instance Field2 (V2 a) (V2 a) a a where _2 = v2y

instance Applicative V2 where
  pure a = V2 a a
  V2 fx fy <*> V2 x y = V2 (fx x) (fy y)

instance Monad V2 where
  V2 x y >>= f = V2 (_v2x $ f x) (_v2y $ f y)

instance Semigroup x => Semigroup (V2 x) where V2 x y <> V2 x' y' = V2 (x <> x') (y <> y')

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

instance Eq1 V2 where liftEq f (V2 x y) (V2 x' y') = f x x' && f y y'

instance Ord1 V2 where liftCompare f (V2 x y) (V2 x' y') = f x x' <> f y y'

instance Show1 V2 where
  liftShowsPrec f _ d (V2 x y) =
    showParen (d > 10) $
      showString "V2 " . f 11 x . showChar ' ' . f 11 y
