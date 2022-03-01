{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | Adapted from [Linear.V3](https://hackage.haskell.org/package/linear-1.21.8/docs/Linear-V3.html)
module Nonlinear.V3 where

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
import Nonlinear.Vector (Vec (..), dot)

#if MIN_VERSION_base(4,14,0)
import GHC.Ix (Ix (..))
#else
import Data.Ix (Ix (..))
#endif

data V3 a = V3 {v3x :: !a, v3y :: !a, v3z :: !a}
  deriving stock (Eq, Show, Read, Bounded, Ord, Functor, Foldable, Traversable, Generic, Generic1, Data, Typeable)

instance Vec V3 where
  construct f = V3 (f _x) (f _y) (f _z)

instance Applicative V3 where
  {-# INLINE pure #-}
  pure a = V3 a a a
  {-# INLINE (<*>) #-}
  V3 fx fy fz <*> V3 x y z = V3 (fx x) (fy y) (fz z)

instance Monad V3 where
  {-# INLINE (>>=) #-}
  V3 x y z >>= f = V3 (v3x $ f x) (v3y $ f y) (v3z $ f z)

instance Semigroup x => Semigroup (V3 x) where V3 x y z <> V3 x' y' z' = V3 (x <> x') (y <> y') (z <> z')

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

instance Eq1 V3 where
  liftEq k (V3 a b c) (V3 d e f) = k a d && k b e && k c f

instance Ord1 V3 where
  liftCompare k (V3 a b c) (V3 d e f) = k a d `mappend` k b e `mappend` k c f

instance Read1 V3 where
  liftReadsPrec k _ d = readParen (d > 10) $ \r ->
    [ (V3 a b c, r4)
      | ("V3", r1) <- lex r,
        (a, r2) <- k 11 r1,
        (b, r3) <- k 11 r2,
        (c, r4) <- k 11 r3
    ]

instance Show1 V3 where
  liftShowsPrec f _ d (V3 a b c) =
    showParen (d > 10) $
      showString "V3 " . f 11 a . showChar ' ' . f 11 b . showChar ' ' . f 11 c

-- | cross product
cross :: Num a => V3 a -> V3 a -> V3 a
cross (V3 a b c) (V3 d e f) = V3 (b * f - c * e) (c * d - a * f) (a * e - b * d)
{-# INLINEABLE cross #-}

-- | scalar triple product
triple :: Num a => V3 a -> V3 a -> V3 a -> a
triple a b c = dot a (cross b c)
{-# INLINE triple #-}

class R2 t => R3 t where
  _z :: Lens' (t a) a
  _xyz :: Lens' (t a) (V3 a)

instance R1 V3 where
  {-# INLINE _x #-}
  _x f (V3 x y z) = (\x' -> V3 x' y z) <$> f x

instance R2 V3 where
  {-# INLINE _y #-}
  _y f (V3 x y z) = (\y' -> V3 x y' z) <$> f y
  {-# INLINE _xy #-}
  _xy f (V3 x y z) = (\(V2 x' y') -> V3 x' y' z) <$> f (V2 x y)

instance R3 V3 where
  {-# INLINE _z #-}
  _z f (V3 x y z) = V3 x y <$> f z
  {-# INLINE _xyz #-}
  _xyz = id

_xz, _yz, _zx, _zy :: R3 t => Lens' (t a) (V2 a)
_xz f = _xyz $ \(V3 a b c) -> f (V2 a c) <&> \(V2 a' c') -> V3 a' b c'
{-# INLINE _xz #-}
_yz f = _xyz $ \(V3 a b c) -> f (V2 b c) <&> \(V2 b' c') -> V3 a b' c'
{-# INLINE _yz #-}
_zx f = _xyz $ \(V3 a b c) -> f (V2 c a) <&> \(V2 c' a') -> V3 a' b c'
{-# INLINE _zx #-}
_zy f = _xyz $ \(V3 a b c) -> f (V2 c b) <&> \(V2 c' b') -> V3 a b' c'
{-# INLINE _zy #-}

_xzy, _yxz, _yzx, _zxy, _zyx :: R3 t => Lens' (t a) (V3 a)
_xzy f = _xyz $ \(V3 a b c) -> f (V3 a c b) <&> \(V3 a' c' b') -> V3 a' b' c'
{-# INLINE _xzy #-}
_yxz f = _xyz $ \(V3 a b c) -> f (V3 b a c) <&> \(V3 b' a' c') -> V3 a' b' c'
{-# INLINE _yxz #-}
_yzx f = _xyz $ \(V3 a b c) -> f (V3 b c a) <&> \(V3 b' c' a') -> V3 a' b' c'
{-# INLINE _yzx #-}
_zxy f = _xyz $ \(V3 a b c) -> f (V3 c a b) <&> \(V3 c' a' b') -> V3 a' b' c'
{-# INLINE _zxy #-}
_zyx f = _xyz $ \(V3 a b c) -> f (V3 c b a) <&> \(V3 c' b' a') -> V3 a' b' c'
{-# INLINE _zyx #-}

instance Storable a => Storable (V3 a) where
  sizeOf _ = 3 * sizeOf (undefined :: a)
  {-# INLINE sizeOf #-}
  alignment _ = alignment (undefined :: a)
  {-# INLINE alignment #-}
  poke ptr (V3 x y z) = do
    poke ptr' x
    pokeElemOff ptr' 1 y
    pokeElemOff ptr' 2 z
    where
      ptr' = castPtr ptr
  {-# INLINE poke #-}
  peek ptr = V3 <$> peek ptr' <*> peekElemOff ptr' 1 <*> peekElemOff ptr' 2
    where
      ptr' = castPtr ptr
  {-# INLINE peek #-}

instance Ix a => Ix (V3 a) where
  {-# SPECIALIZE instance Ix (V3 Int) #-}

  range (V3 l1 l2 l3, V3 u1 u2 u3) =
    [ V3 i1 i2 i3 | i1 <- range (l1, u1), i2 <- range (l2, u2), i3 <- range (l3, u3)
    ]
  {-# INLINE range #-}

  inRange (V3 l1 l2 l3, V3 u1 u2 u3) (V3 i1 i2 i3) =
    inRange (l1, u1) i1 && inRange (l2, u2) i2
      && inRange (l3, u3) i3
  {-# INLINE inRange #-}

#if MIN_VERSION_base(4,14,0)
  unsafeIndex (V3 l1 l2 l3, V3 u1 u2 u3) (V3 i1 i2 i3) =
    unsafeIndex (l3, u3) i3 + unsafeRangeSize (l3, u3)
      * ( unsafeIndex (l2, u2) i2 + unsafeRangeSize (l2, u2)
            * unsafeIndex (l1, u1) i1
        )
  {-# INLINE unsafeIndex #-}
#else
  index (V3 l1 l2 l3, V3 u1 u2 u3) (V3 i1 i2 i3) =
    index (l3, u3) i3 + rangeSize (l3, u3)
      * ( index (l2, u2) i2 + rangeSize (l2, u2)
            * index (l1, u1) i1
        )
  {-# INLINE index #-}
#endif
