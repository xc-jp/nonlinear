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
{-# LANGUAGE TypeFamilies #-}

module Nonlinear.V4 where

import Control.Applicative
import Data.Data (Data, Typeable)
import Data.Functor ((<&>))
import Data.Functor.Classes
import GHC.Generics (Generic, Generic1)
import Lens.Micro.Extras (view)
import Lens.Micro.Internal (Field1 (..), Field2 (..), Field3 (..), Field4 (..))
import Lens.Micro.Type (Lens')
import Nonlinear.Distributive (Distributive (distribute))
import Nonlinear.Representable
import Nonlinear.V1 (R1 (..))
import Nonlinear.V2 (R2 (..), V2 (..))
import Nonlinear.V3 (R3 (..), V3 (..))
import Nonlinear.Vector ((*^))

-- TODO field accessors are nice, but the derived show instance is not.
-- Either we drop the accessors, or we manually write the Show instance.
data V4 a = V4 {v4x :: !a, v4y :: !a, v4z :: !a, v4w :: !a}
  deriving stock (Eq, Show, Bounded, Ord, Functor, Foldable, Traversable, Generic, Generic1, Data, Typeable)

instance Distributive V4 where
  distribute f = V4 (fmap v4x f) (fmap v4y f) (fmap v4z f) (fmap v4w f)
  {-# INLINE distribute #-}

instance Representable V4 where
  type Rep V4 = E V4
  tabulate f = V4 (f $ E _x) (f $ E _y) (f $ E _z) (f $ E _w)
  {-# INLINE tabulate #-}
  index xs (E l) = view l xs
  {-# INLINE index #-}

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

-- | Convert a 3-dimensional affine vector into a 4-dimensional homogeneous vector,
-- i.e. sets the @w@ coordinate to 0.
vector :: Num a => V3 a -> V4 a
vector (V3 a b c) = V4 a b c 0
{-# INLINE vector #-}

-- | Convert a 3-dimensional affine point into a 4-dimensional homogeneous vector,
-- i.e. sets the @w@ coordinate to 1.
point :: Num a => V3 a -> V4 a
point (V3 a b c) = V4 a b c 1
{-# INLINE point #-}

-- | Convert 4-dimensional projective coordinates to a 3-dimensional
-- point. This operation may be denoted, @euclidean [x:y:z:w] = (x\/w,
-- y\/w, z\/w)@ where the projective, homogenous, coordinate
-- @[x:y:z:w]@ is one of many associated with a single point @(x\/w,
-- y\/w, z\/w)@.
normalizePoint :: Fractional a => V4 a -> V3 a
normalizePoint (V4 a b c w) = (1 / w) *^ V3 a b c
{-# INLINE normalizePoint #-}

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

_xw, _yw, _zw, _wx, _wy, _wz :: R4 t => Lens' (t a) (V2 a)
_xw f = _xyzw $ \(V4 a b c d) -> f (V2 a d) <&> \(V2 a' d') -> V4 a' b c d'
{-# INLINE _xw #-}
_yw f = _xyzw $ \(V4 a b c d) -> f (V2 b d) <&> \(V2 b' d') -> V4 a b' c d'
{-# INLINE _yw #-}
_zw f = _xyzw $ \(V4 a b c d) -> f (V2 c d) <&> \(V2 c' d') -> V4 a b c' d'
{-# INLINE _zw #-}
_wx f = _xyzw $ \(V4 a b c d) -> f (V2 d a) <&> \(V2 d' a') -> V4 a' b c d'
{-# INLINE _wx #-}
_wy f = _xyzw $ \(V4 a b c d) -> f (V2 d b) <&> \(V2 d' b') -> V4 a b' c d'
{-# INLINE _wy #-}
_wz f = _xyzw $ \(V4 a b c d) -> f (V2 d c) <&> \(V2 d' c') -> V4 a b c' d'
{-# INLINE _wz #-}

_xyw, _xzw, _xwy, _xwz, _yxw, _yzw, _ywx, _ywz, _zxw, _zyw, _zwx, _zwy, _wxy, _wxz, _wyx, _wyz, _wzx, _wzy :: R4 t => Lens' (t a) (V3 a)
_xyw f = _xyzw $ \(V4 a b c d) -> f (V3 a b d) <&> \(V3 a' b' d') -> V4 a' b' c d'
{-# INLINE _xyw #-}
_xzw f = _xyzw $ \(V4 a b c d) -> f (V3 a c d) <&> \(V3 a' c' d') -> V4 a' b c' d'
{-# INLINE _xzw #-}
_xwy f = _xyzw $ \(V4 a b c d) -> f (V3 a d b) <&> \(V3 a' d' b') -> V4 a' b' c d'
{-# INLINE _xwy #-}
_xwz f = _xyzw $ \(V4 a b c d) -> f (V3 a d c) <&> \(V3 a' d' c') -> V4 a' b c' d'
{-# INLINE _xwz #-}
_yxw f = _xyzw $ \(V4 a b c d) -> f (V3 b a d) <&> \(V3 b' a' d') -> V4 a' b' c d'
{-# INLINE _yxw #-}
_yzw f = _xyzw $ \(V4 a b c d) -> f (V3 b c d) <&> \(V3 b' c' d') -> V4 a b' c' d'
{-# INLINE _yzw #-}
_ywx f = _xyzw $ \(V4 a b c d) -> f (V3 b d a) <&> \(V3 b' d' a') -> V4 a' b' c d'
{-# INLINE _ywx #-}
_ywz f = _xyzw $ \(V4 a b c d) -> f (V3 b d c) <&> \(V3 b' d' c') -> V4 a b' c' d'
{-# INLINE _ywz #-}
_zxw f = _xyzw $ \(V4 a b c d) -> f (V3 c a d) <&> \(V3 c' a' d') -> V4 a' b c' d'
{-# INLINE _zxw #-}
_zyw f = _xyzw $ \(V4 a b c d) -> f (V3 c b d) <&> \(V3 c' b' d') -> V4 a b' c' d'
{-# INLINE _zyw #-}
_zwx f = _xyzw $ \(V4 a b c d) -> f (V3 c d a) <&> \(V3 c' d' a') -> V4 a' b c' d'
{-# INLINE _zwx #-}
_zwy f = _xyzw $ \(V4 a b c d) -> f (V3 c d b) <&> \(V3 c' d' b') -> V4 a b' c' d'
{-# INLINE _zwy #-}
_wxy f = _xyzw $ \(V4 a b c d) -> f (V3 d a b) <&> \(V3 d' a' b') -> V4 a' b' c d'
{-# INLINE _wxy #-}
_wxz f = _xyzw $ \(V4 a b c d) -> f (V3 d a c) <&> \(V3 d' a' c') -> V4 a' b c' d'
{-# INLINE _wxz #-}
_wyx f = _xyzw $ \(V4 a b c d) -> f (V3 d b a) <&> \(V3 d' b' a') -> V4 a' b' c d'
{-# INLINE _wyx #-}
_wyz f = _xyzw $ \(V4 a b c d) -> f (V3 d b c) <&> \(V3 d' b' c') -> V4 a b' c' d'
{-# INLINE _wyz #-}
_wzx f = _xyzw $ \(V4 a b c d) -> f (V3 d c a) <&> \(V3 d' c' a') -> V4 a' b c' d'
{-# INLINE _wzx #-}
_wzy f = _xyzw $ \(V4 a b c d) -> f (V3 d c b) <&> \(V3 d' c' b') -> V4 a b' c' d'
{-# INLINE _wzy #-}

_xywz, _xzyw, _xzwy, _xwyz, _xwzy, _yxzw, _yxwz, _yzxw, _yzwx, _ywxz, _ywzx, _zxyw, _zxwy, _zyxw, _zywx, _zwxy, _zwyx, _wxyz, _wxzy, _wyxz, _wyzx, _wzxy, _wzyx :: R4 t => Lens' (t a) (V4 a)
_xywz f = _xyzw $ \(V4 a b c d) -> f (V4 a b d c) <&> \(V4 a' b' d' c') -> V4 a' b' c' d'
{-# INLINE _xywz #-}
_xzyw f = _xyzw $ \(V4 a b c d) -> f (V4 a c b d) <&> \(V4 a' c' b' d') -> V4 a' b' c' d'
{-# INLINE _xzyw #-}
_xzwy f = _xyzw $ \(V4 a b c d) -> f (V4 a c d b) <&> \(V4 a' c' d' b') -> V4 a' b' c' d'
{-# INLINE _xzwy #-}
_xwyz f = _xyzw $ \(V4 a b c d) -> f (V4 a d b c) <&> \(V4 a' d' b' c') -> V4 a' b' c' d'
{-# INLINE _xwyz #-}
_xwzy f = _xyzw $ \(V4 a b c d) -> f (V4 a d c b) <&> \(V4 a' d' c' b') -> V4 a' b' c' d'
{-# INLINE _xwzy #-}
_yxzw f = _xyzw $ \(V4 a b c d) -> f (V4 b a c d) <&> \(V4 b' a' c' d') -> V4 a' b' c' d'
{-# INLINE _yxzw #-}
_yxwz f = _xyzw $ \(V4 a b c d) -> f (V4 b a d c) <&> \(V4 b' a' d' c') -> V4 a' b' c' d'
{-# INLINE _yxwz #-}
_yzxw f = _xyzw $ \(V4 a b c d) -> f (V4 b c a d) <&> \(V4 b' c' a' d') -> V4 a' b' c' d'
{-# INLINE _yzxw #-}
_yzwx f = _xyzw $ \(V4 a b c d) -> f (V4 b c d a) <&> \(V4 b' c' d' a') -> V4 a' b' c' d'
{-# INLINE _yzwx #-}
_ywxz f = _xyzw $ \(V4 a b c d) -> f (V4 b d a c) <&> \(V4 b' d' a' c') -> V4 a' b' c' d'
{-# INLINE _ywxz #-}
_ywzx f = _xyzw $ \(V4 a b c d) -> f (V4 b d c a) <&> \(V4 b' d' c' a') -> V4 a' b' c' d'
{-# INLINE _ywzx #-}
_zxyw f = _xyzw $ \(V4 a b c d) -> f (V4 c a b d) <&> \(V4 c' a' b' d') -> V4 a' b' c' d'
{-# INLINE _zxyw #-}
_zxwy f = _xyzw $ \(V4 a b c d) -> f (V4 c a d b) <&> \(V4 c' a' d' b') -> V4 a' b' c' d'
{-# INLINE _zxwy #-}
_zyxw f = _xyzw $ \(V4 a b c d) -> f (V4 c b a d) <&> \(V4 c' b' a' d') -> V4 a' b' c' d'
{-# INLINE _zyxw #-}
_zywx f = _xyzw $ \(V4 a b c d) -> f (V4 c b d a) <&> \(V4 c' b' d' a') -> V4 a' b' c' d'
{-# INLINE _zywx #-}
_zwxy f = _xyzw $ \(V4 a b c d) -> f (V4 c d a b) <&> \(V4 c' d' a' b') -> V4 a' b' c' d'
{-# INLINE _zwxy #-}
_zwyx f = _xyzw $ \(V4 a b c d) -> f (V4 c d b a) <&> \(V4 c' d' b' a') -> V4 a' b' c' d'
{-# INLINE _zwyx #-}
_wxyz f = _xyzw $ \(V4 a b c d) -> f (V4 d a b c) <&> \(V4 d' a' b' c') -> V4 a' b' c' d'
{-# INLINE _wxyz #-}
_wxzy f = _xyzw $ \(V4 a b c d) -> f (V4 d a c b) <&> \(V4 d' a' c' b') -> V4 a' b' c' d'
{-# INLINE _wxzy #-}
_wyxz f = _xyzw $ \(V4 a b c d) -> f (V4 d b a c) <&> \(V4 d' b' a' c') -> V4 a' b' c' d'
{-# INLINE _wyxz #-}
_wyzx f = _xyzw $ \(V4 a b c d) -> f (V4 d b c a) <&> \(V4 d' b' c' a') -> V4 a' b' c' d'
{-# INLINE _wyzx #-}
_wzxy f = _xyzw $ \(V4 a b c d) -> f (V4 d c a b) <&> \(V4 d' c' a' b') -> V4 a' b' c' d'
{-# INLINE _wzxy #-}
_wzyx f = _xyzw $ \(V4 a b c d) -> f (V4 d c b a) <&> \(V4 d' c' b' a') -> V4 a' b' c' d'
{-# INLINE _wzyx #-}
