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
import Data.Functor.Classes
import GHC.Generics (Generic, Generic1)
import Lens.Micro.Internal (Field1 (..), Field2 (..), Field3 (..), Field4 (..))
import Lens.Micro.Type (Lens')
import Nonlinear.V1 (R1 (..))
import Nonlinear.V2 (R2 (..), V2 (..))
import Nonlinear.V3 (R3 (..), V3 (..))

data V4 a = V4 {v4x :: !a, v4y :: !a, v4z :: !a, v4w :: !a}
  deriving stock (Eq, Show, Bounded, Ord, Functor, Foldable, Traversable, Generic, Generic1, Data, Typeable)

instance Field1 (V4 a) (V4 a) a a where
  {-# INLINE _1 #-}
  _1 f (V4 x y z w) = (\x' -> V4 x' y z w) <$> f x

instance Field2 (V4 a) (V4 a) a a where
  {-# INLINE _2 #-}
  _2 f (V4 x y z w) = (\y' -> V4 x y' z w) <$> f y

instance Field3 (V4 a) (V4 a) a a where
  {-# INLINE _3 #-}
  _3 f (V4 x y z w) = (\z' -> V4 x y z' w) <$> f z

instance Field4 (V4 a) (V4 a) a a where
  {-# INLINE _4 #-}
  _4 f (V4 x y z w) = V4 x y z <$> f w

instance Applicative V4 where
  pure a = V4 a a a a
  V4 fx fy fz fw <*> V4 x y z w = V4 (fx x) (fy y) (fz z) (fw w)

instance Monad V4 where
  V4 x y z w >>= f = V4 (v4x $ f x) (v4y $ f y) (v4z $ f z) (v4w $ f w)

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

class R3 t => R4 t where
  _w :: Lens' (t a) a
  _xyzw :: Lens' (t a) (V4 a)

instance R1 V4 where
  {-# INLINE _x #-}
  _x = _1

instance R2 V4 where
  {-# INLINE _y #-}
  _y = _2
  {-# INLINE _xy #-}
  _xy f (V4 x y z w) = (\(V2 x' y') -> V4 x' y' z w) <$> f (V2 x y)

instance R3 V4 where
  {-# INLINE _z #-}
  _z = _3
  {-# INLINE _xyz #-}
  _xyz f (V4 x y z w) = (\(V3 x' y' z') -> V4 x' y' z' w) <$> f (V3 x y z)

instance R4 V4 where
  {-# INLINE _w #-}
  _w = _4
  {-# INLINE _xyzw #-}
  _xyzw = id
