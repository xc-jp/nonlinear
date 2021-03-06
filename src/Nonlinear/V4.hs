{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | Adapted from [Linear.V4](https://hackage.haskell.org/package/linear-1.21.8/docs/Linear-V4.html)
module Nonlinear.V4 where

import Control.Applicative
import Data.Data (Data, Typeable)
import Data.Functor ((<&>))
import Data.Functor.Classes
import Foreign (Storable (..))
import Foreign.Ptr (castPtr)
import GHC.Generics (Generic, Generic1)
import Nonlinear.Internal (Lens')
import Nonlinear.V1 (R1 (..))
import Nonlinear.V2 (R2 (..), V2 (..))
import Nonlinear.V3 (R3 (..), V3 (..))
import Nonlinear.Vector (Vec (..))

#if MIN_VERSION_base(4,14,0)
import GHC.Ix (Ix (..))
#else
import Data.Ix (Ix (..))
#endif

-- TODO field accessors are nice, but the derived show instance is not.
-- Either we drop the accessors, or we manually write the Show instance.
-- Note that Show1 is already hand-rolled
data V4 a = V4 {v4x :: !a, v4y :: !a, v4z :: !a, v4w :: !a}
  deriving stock (Eq, Show, Read, Bounded, Ord, Functor, Foldable, Traversable, Generic, Generic1, Data, Typeable)

instance Vec V4 where
  construct f = V4 (f _x) (f _y) (f _z) (f _w)

instance Applicative V4 where
  {-# INLINE pure #-}
  pure a = V4 a a a a
  {-# INLINE (<*>) #-}
  V4 fx fy fz fw <*> V4 x y z w = V4 (fx x) (fy y) (fz z) (fw w)

instance Monad V4 where
  {-# INLINE (>>=) #-}
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

instance Eq1 V4 where
  liftEq k (V4 a b c d) (V4 e f g h) = k a e && k b f && k c g && k d h

instance Ord1 V4 where
  liftCompare k (V4 a b c d) (V4 e f g h) = k a e <> k b f <> k c g <> k d h

instance Read1 V4 where
  liftReadsPrec k _ z = readParen (z > 10) $ \r ->
    [ (V4 a b c d, r5)
      | ("V4", r1) <- lex r,
        (a, r2) <- k 11 r1,
        (b, r3) <- k 11 r2,
        (c, r4) <- k 11 r3,
        (d, r5) <- k 11 r4
    ]

instance Show1 V4 where
  liftShowsPrec f _ z (V4 a b c d) =
    showParen (z > 10) $
      showString "V4 " . f 11 a . showChar ' ' . f 11 b . showChar ' ' . f 11 c . showChar ' ' . f 11 d

class R3 t => R4 t where
  _w :: Lens' (t a) a
  _xyzw :: Lens' (t a) (V4 a)

instance R1 V4 where
  {-# INLINE _x #-}
  _x f (V4 x y z w) = (\x' -> V4 x' y z w) <$> f x

instance R2 V4 where
  {-# INLINE _y #-}
  _y f (V4 x y z w) = (\y' -> V4 x y' z w) <$> f y
  {-# INLINE _xy #-}
  _xy f (V4 x y z w) = (\(V2 x' y') -> V4 x' y' z w) <$> f (V2 x y)

instance R3 V4 where
  {-# INLINE _z #-}
  _z f (V4 x y z w) = (\z' -> V4 x y z' w) <$> f z
  {-# INLINE _xyz #-}
  _xyz f (V4 x y z w) = (\(V3 x' y' z') -> V4 x' y' z' w) <$> f (V3 x y z)

instance R4 V4 where
  {-# INLINE _w #-}
  _w f (V4 x y z w) = V4 x y z <$> f w
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

instance Storable a => Storable (V4 a) where
  sizeOf _ = 4 * sizeOf (undefined :: a)
  {-# INLINE sizeOf #-}
  alignment _ = alignment (undefined :: a)
  {-# INLINE alignment #-}
  poke ptr (V4 x y z w) = do
    poke ptr' x
    pokeElemOff ptr' 1 y
    pokeElemOff ptr' 2 z
    pokeElemOff ptr' 3 w
    where
      ptr' = castPtr ptr
  {-# INLINE poke #-}
  peek ptr =
    V4 <$> peek ptr' <*> peekElemOff ptr' 1
      <*> peekElemOff ptr' 2
      <*> peekElemOff ptr' 3
    where
      ptr' = castPtr ptr
  {-# INLINE peek #-}

instance Ix a => Ix (V4 a) where
  {-# SPECIALIZE instance Ix (V4 Int) #-}

  range (V4 l1 l2 l3 l4, V4 u1 u2 u3 u4) =
    [ V4 i1 i2 i3 i4 | i1 <- range (l1, u1), i2 <- range (l2, u2), i3 <- range (l3, u3), i4 <- range (l4, u4)
    ]
  {-# INLINE range #-}

  inRange (V4 l1 l2 l3 l4, V4 u1 u2 u3 u4) (V4 i1 i2 i3 i4) =
    inRange (l1, u1) i1 && inRange (l2, u2) i2
      && inRange (l3, u3) i3
      && inRange (l4, u4) i4
  {-# INLINE inRange #-}

#if MIN_VERSION_base(4,14,0)
  unsafeIndex (V4 l1 l2 l3 l4, V4 u1 u2 u3 u4) (V4 i1 i2 i3 i4) =
    unsafeIndex (l4, u4) i4 + unsafeRangeSize (l4, u4)
      * ( unsafeIndex (l3, u3) i3 + unsafeRangeSize (l3, u3)
            * ( unsafeIndex (l2, u2) i2 + unsafeRangeSize (l2, u2)
                  * unsafeIndex (l1, u1) i1
              )
        )
  {-# INLINE unsafeIndex #-}
#else
  index (V4 l1 l2 l3 l4, V4 u1 u2 u3 u4) (V4 i1 i2 i3 i4) =
    index (l4, u4) i4 + rangeSize (l4, u4)
      * ( index (l3, u3) i3 + rangeSize (l3, u3)
            * ( index (l2, u2) i2 + rangeSize (l2, u2)
                  * index (l1, u1) i1
              )
        )
  {-# INLINE index #-}
#endif
