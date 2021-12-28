{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | Adapted from [Linear.V2](https://hackage.haskell.org/package/linear-1.21.8/docs/Linear-V2.html)
module Nonlinear.V2 where

import Control.Applicative
import Data.Data (Data, Typeable)
import Data.Functor ((<&>))
import Data.Functor.Classes
import Foreign (Storable (..))
import Foreign.Ptr (castPtr)
import GHC.Generics (Generic, Generic1)
import GHC.Ix (Ix (..))
import Nonlinear.Internal (Lens')
import Nonlinear.V1 (R1 (..))
import Nonlinear.Vector (norm)

data V2 a = V2 {v2x :: !a, v2y :: !a}
  deriving stock (Eq, Show, Read, Bounded, Ord, Functor, Foldable, Traversable, Generic, Generic1, Data, Typeable)

instance Applicative V2 where
  {-# INLINE pure #-}
  {-# INLINE (<*>) #-}
  pure a = V2 a a
  V2 fx fy <*> V2 x y = V2 (fx x) (fy y)

instance Monad V2 where
  {-# INLINE (>>=) #-}
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

instance Eq1 V2 where
  liftEq f (V2 a b) (V2 c d) = f a c && f b d

instance Ord1 V2 where
  liftCompare f (V2 a b) (V2 c d) = f a c `mappend` f b d

instance Read1 V2 where
  liftReadsPrec f _ = readsData $ readsBinaryWith f f "V2" V2

instance Show1 V2 where
  liftShowsPrec f _ d (V2 a b) = showsBinaryWith f f "V2" d a b

-- | the counter-clockwise perpendicular vector
--
-- >>> perp $ V2 10 20
-- V2 (-20) 10
perp :: Num a => V2 a -> V2 a
perp (V2 a b) = V2 (negate b) a
{-# INLINE perp #-}

angle :: Floating a => a -> V2 a
angle a = V2 (cos a) (sin a)

unangle :: (Floating a, Ord a) => V2 a -> a
unangle a@(V2 ax ay) =
  let alpha = asin $ ay / norm a
   in if ax < 0
        then pi - alpha
        else alpha

-- | The Z-component of the cross product of two vectors in the XY-plane.
--
-- >>> crossZ (V2 1 0) (V2 0 1)
-- 1
crossZ :: Num a => V2 a -> V2 a -> a
crossZ (V2 x1 y1) (V2 x2 y2) = x1 * y2 - y1 * x2
{-# INLINE crossZ #-}

class R1 t => R2 t where
  _y :: Lens' (t a) a
  _xy :: Lens' (t a) (V2 a)

instance R1 V2 where
  {-# INLINE _x #-}
  _x f (V2 x y) = (\x' -> V2 x' y) <$> f x

instance R2 V2 where
  {-# INLINE _y #-}
  _y f (V2 x y) = V2 x <$> f y
  {-# INLINE _xy #-}
  _xy = id

-- |
-- >>> V2 1 2 ^. _yx
-- V2 2 1
_yx :: R2 t => Lens' (t a) (V2 a)
_yx f = _xy $ \(V2 a b) -> f (V2 b a) <&> \(V2 b' a') -> V2 a' b'
{-# INLINE _yx #-}

instance Storable a => Storable (V2 a) where
  sizeOf _ = 2 * sizeOf (undefined :: a)
  {-# INLINE sizeOf #-}
  alignment _ = alignment (undefined :: a)
  {-# INLINE alignment #-}
  poke ptr (V2 x y) = poke ptr' x >> pokeElemOff ptr' 1 y
    where
      ptr' = castPtr ptr
  {-# INLINE poke #-}
  peek ptr = V2 <$> peek ptr' <*> peekElemOff ptr' 1
    where
      ptr' = castPtr ptr
  {-# INLINE peek #-}

instance Ix a => Ix (V2 a) where
  {-# SPECIALIZE instance Ix (V2 Int) #-}

  range (V2 l1 l2, V2 u1 u2) =
    [V2 i1 i2 | i1 <- range (l1, u1), i2 <- range (l2, u2)]
  {-# INLINE range #-}

  unsafeIndex (V2 l1 l2, V2 u1 u2) (V2 i1 i2) =
    unsafeIndex (l1, u1) i1 * unsafeRangeSize (l2, u2) + unsafeIndex (l2, u2) i2
  {-# INLINE unsafeIndex #-}

  inRange (V2 l1 l2, V2 u1 u2) (V2 i1 i2) =
    inRange (l1, u1) i1 && inRange (l2, u2) i2
  {-# INLINE inRange #-}
