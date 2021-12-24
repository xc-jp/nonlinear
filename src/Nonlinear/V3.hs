{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}

-- | Adapted from [Linear.V3](https://hackage.haskell.org/package/linear-1.21.8/docs/Linear-V3.html)
module Nonlinear.V3 where

import Control.Applicative
import Data.Data (Data, Typeable)
import Data.Functor ((<&>))
import Data.Functor.Classes
import GHC.Generics (Generic, Generic1)
import Nonlinear.Distributive (Distributive (distribute))
import Nonlinear.Internal (Lens', view)
import Nonlinear.Representable
import Nonlinear.V1 (R1 (..))
import Nonlinear.V2 (R2 (..), V2 (..))

data V3 a = V3 {v3x :: !a, v3y :: !a, v3z :: !a}
  deriving stock (Eq, Show, Bounded, Ord, Functor, Foldable, Traversable, Generic, Generic1, Data, Typeable)

instance Distributive V3 where
  distribute f = V3 (fmap v3x f) (fmap v3y f) (fmap v3z f)
  {-# INLINE distribute #-}

instance Representable V3 where
  type Rep V3 = E V3
  tabulate f = V3 (f $ E _x) (f $ E _y) (f $ E _z)
  {-# INLINE tabulate #-}
  index xs (E l) = view l xs
  {-# INLINE index #-}

instance Applicative V3 where
  pure a = V3 a a a
  V3 fx fy fz <*> V3 x y z = V3 (fx x) (fy y) (fz z)

instance Monad V3 where
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

instance Eq1 V3 where liftEq f (V3 x y z) (V3 x' y' z') = f x x' && f y y' && f z z'

instance Ord1 V3 where liftCompare f (V3 x y z) (V3 x' y' z') = f x x' <> f y y' <> f z z'

instance Show1 V3 where
  liftShowsPrec f _ d (V3 x y z) =
    showParen (d > 10) $
      showString "V3 " . f 11 x . showChar ' ' . f 11 y . showChar ' ' . f 11 z

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
