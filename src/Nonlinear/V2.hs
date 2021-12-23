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
import Data.Functor ((<&>))
import Data.Functor.Classes
import GHC.Generics (Generic, Generic1)
import Lens.Micro.Internal (Field1 (..), Field2 (..))
import Lens.Micro.Type (Lens')
import Nonlinear.Distributive (Distributive (distribute))
import Nonlinear.V1 (R1 (..))

data V2 a = V2 {v2x :: !a, v2y :: !a}
  deriving stock (Eq, Show, Bounded, Ord, Functor, Foldable, Traversable, Generic, Generic1, Data, Typeable)

instance Distributive V2 where
  distribute f = V2 (fmap v2x f) (fmap v2y f)
  {-# INLINE distribute #-}

instance Field1 (V2 a) (V2 a) a a where
  {-# INLINE _1 #-}
  _1 f (V2 x y) = (\x' -> V2 x' y) <$> f x

instance Field2 (V2 a) (V2 a) a a where
  {-# INLINE _2 #-}
  _2 f (V2 x y) = V2 x <$> f y

instance Applicative V2 where
  pure a = V2 a a
  V2 fx fy <*> V2 x y = V2 (fx x) (fy y)

instance Monad V2 where
  V2 x y >>= f = V2 (v2x $ f x) (v2y $ f y)

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

class R1 t => R2 t where
  _y :: Lens' (t a) a
  _xy :: Lens' (t a) (V2 a)

instance R1 V2 where
  {-# INLINE _x #-}
  _x = _1

instance R2 V2 where
  {-# INLINE _y #-}
  _y = _1
  {-# INLINE _xy #-}
  _xy = id

-- |
-- >>> V2 1 2 ^. _yx
-- V2 2 1
_yx :: R2 t => Lens' (t a) (V2 a)
_yx f = _xy $ \(V2 a b) -> f (V2 b a) <&> \(V2 b' a') -> V2 a' b'
{-# INLINE _yx #-}
