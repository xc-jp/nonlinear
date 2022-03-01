{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}

-- | Adapted from [Linear.Matrix](https://hackage.haskell.org/package/linear-1.21.8/docs/Linear-Matrix.html)
module Nonlinear.Matrix
  ( (!*!),
    (!*),
    (*!),
    (!!*),
    (*!!),
    (!!/),
    column,
    diagonal,
    trace,
    M22,
    M23,
    M24,
    M32,
    M33,
    M34,
    M42,
    M43,
    M44,
    m33_to_m44,
    m43_to_m44,
    det22,
    det33,
    det44,
    inv22,
    inv33,
    inv44,
    identity,
    translation,
    transpose,
    fromQuaternion,
    mkTransformation,
    mkTransformationMat,
    _m22,
    _m23,
    _m24,
    _m32,
    _m33,
    _m34,
    _m42,
    _m43,
    _m44,
  )
where

import Control.Applicative
import Control.Monad (join)
import Data.Foldable as Foldable
import Nonlinear.Internal (Lens', lens, set, view)
import Nonlinear.Quaternion
import Nonlinear.V2
import Nonlinear.V3
import Nonlinear.V4
import Nonlinear.Vector (Vec, scaled, (*^))

-- | This is more restrictive than linear's @LensLike (Context a b) s t a b -> Lens (f s) (f t) (f a) (f b)@, but in return we get a much simpler implementation which should suffice in 99% of cases.
column ::
  Vec v =>
  Lens' a b ->
  Lens' (v a) (v b)
column l = lens (fmap $ view l) (liftA2 (flip $ set l))
{-# INLINE column #-}

diagonal :: Vec v => v (v a) -> v a
diagonal = join

trace :: (Vec v, Num a) => v (v a) -> a
trace = sum . diagonal

infixl 7 !*!

-- | Matrix product
--
-- >>> V2 (V3 1 2 3) (V3 4 5 6) !*! V3 (V2 1 2) (V2 3 4) (V2 4 5)
-- V2 (V2 19 25) (V2 43 58)
(!*!) :: (Vec m, Vec t, Vec n, Num a) => m (t a) -> t (n a) -> m (n a)
f !*! g = fmap (\f' -> Foldable.foldl' (liftA2 (+)) (pure 0) $ liftA2 (*^) f' g) f

infixl 7 !*

-- | Matrix * column vector
--
-- >>> V2 (V3 1 2 3) (V3 4 5 6) !* V3 7 8 9
-- V2 50 122
(!*) :: (Vec m, Vec r, Num a) => m (r a) -> r a -> m a
m !* v = fmap (\r -> Foldable.sum $ liftA2 (*) r v) m

infixl 7 *!

-- | Row vector * matrix
--
-- >>> V2 1 2 *! V2 (V3 3 4 5) (V3 6 7 8)
-- V3 15 18 21

-- (*!) :: (Metric r, Additive n, Num a) => r a -> r (n a) -> n a
-- f *! g = dot f <$> distribute g

(*!) :: (Vec f, Vec t, Num a, Num (f a)) => t a -> t (f a) -> f a
f *! g = sum $ liftA2 (*^) f g

infixl 7 *!!

-- | Scalar-matrix product
--
-- >>> 5 *!! V2 (V2 1 2) (V2 3 4)
-- V2 (V2 5 10) (V2 15 20)
(*!!) :: (Vec m, Vec r, Num a) => a -> m (r a) -> m (r a)
s *!! m = fmap (s *^) m
{-# INLINE (*!!) #-}

infixl 7 !!*

-- | Matrix-scalar product
--
-- >>> V2 (V2 1 2) (V2 3 4) !!* 5
-- V2 (V2 5 10) (V2 15 20)
(!!*) :: (Vec m, Vec r, Num a) => m (r a) -> a -> m (r a)
(!!*) = flip (*!!)
{-# INLINE (!!*) #-}

infixl 7 !!/

-- | Matrix-scalar division
(!!/) :: (Vec r, Vec m, Fractional (r a), Fractional a) => m (r a) -> a -> m (r a)
m !!/ s = fmap (/ pure s) m
{-# INLINE (!!/) #-}

-- * Matrices

-- | A 2x2 matrix with row-major representation
type M22 a = V2 (V2 a)

-- | A 2x3 matrix with row-major representation
type M23 a = V2 (V3 a)

-- | A 2x4 matrix with row-major representation
type M24 a = V2 (V4 a)

-- | A 3x2 matrix with row-major representation
type M32 a = V3 (V2 a)

-- | A 3x3 matrix with row-major representation
type M33 a = V3 (V3 a)

-- | A 3x4 matrix with row-major representation
type M34 a = V3 (V4 a)

-- | A 4x2 matrix with row-major representation
type M42 a = V4 (V2 a)

-- | A 4x3 matrix with row-major representation
type M43 a = V4 (V3 a)

-- | A 4x4 matrix with row-major representation
type M44 a = V4 (V4 a)

-- | Build a rotation matrix from a unit 'Quaternion'.
fromQuaternion :: Num a => Quaternion a -> M33 a
fromQuaternion (Quaternion w (V3 x y z)) =
  V3
    (V3 (1 - 2 * (y2 + z2)) (2 * (xy - zw)) (2 * (xz + yw)))
    (V3 (2 * (xy + zw)) (1 - 2 * (x2 + z2)) (2 * (yz - xw)))
    (V3 (2 * (xz - yw)) (2 * (yz + xw)) (1 - 2 * (x2 + y2)))
  where
    x2 = x * x
    y2 = y * y
    z2 = z * z
    xy = x * y
    xz = x * z
    xw = x * w
    yz = y * z
    yw = y * w
    zw = z * w
{-# INLINE fromQuaternion #-}

-- | Build a transformation matrix from a rotation matrix and a
-- translation vector.
mkTransformationMat :: Num a => M33 a -> V3 a -> M44 a
mkTransformationMat (V3 r1 r2 r3) (V3 tx ty tz) =
  V4 (snoc3 r1 tx) (snoc3 r2 ty) (snoc3 r3 tz) (V4 0 0 0 1)
  where
    snoc3 (V3 x y z) = V4 x y z
{-# INLINE mkTransformationMat #-}

-- | Build a transformation matrix from a rotation expressed as a
--  'Quaternion' and a translation vector.
mkTransformation :: Num a => Quaternion a -> V3 a -> M44 a
mkTransformation = mkTransformationMat . fromQuaternion
{-# INLINE mkTransformation #-}

-- | Convert from a 4x3 matrix to a 4x4 matrix, extending it with the @[ 0 0 0 1 ]@ column vector
m43_to_m44 :: Num a => M43 a -> M44 a
m43_to_m44
  ( V4
      (V3 a b c)
      (V3 d e f)
      (V3 g h i)
      (V3 j k l)
    ) =
    V4
      (V4 a b c 0)
      (V4 d e f 0)
      (V4 g h i 0)
      (V4 j k l 1)

-- | Convert a 3x3 matrix to a 4x4 matrix extending it with 0's in the new row and column.
m33_to_m44 :: Num a => M33 a -> M44 a
m33_to_m44 (V3 r1 r2 r3) = V4 (vector r1) (vector r2) (vector r3) (point 0)

-- | The identity matrix for any dimension vector.
--
--  >>> identity :: M44 Int
--  V4 (V4 1 0 0 0) (V4 0 1 0 0) (V4 0 0 1 0) (V4 0 0 0 1)
--  >>> identity :: V3 (V3 Int)
--  V3 (V3 1 0 0) (V3 0 1 0) (V3 0 0 1)
identity :: (Vec v, Num a) => v (v a)
identity = scaled (pure 1)
{-# INLINE identity #-}

-- | Extract the translation vector (first three entries of the last
--  column) from a 3x4 or 4x4 matrix.
translation :: (Vec t, R3 t, R4 v) => Lens' (t (v a)) (V3 a)
translation = column _w . _xyz

-- | Extract a 2x2 matrix from a matrix of higher dimensions by dropping excess
--  rows and columns.
_m22 :: (Vec t, R2 t, R2 v) => Lens' (t (v a)) (M22 a)
_m22 = column _xy . _xy

-- | Extract a 2x3 matrix from a matrix of higher dimensions by dropping excess
--  rows and columns.
_m23 :: (Vec t, R2 t, R3 v) => Lens' (t (v a)) (M23 a)
_m23 = column _xyz . _xy

-- | Extract a 2x4 matrix from a matrix of higher dimensions by dropping excess
--  rows and columns.
_m24 :: (Vec t, R2 t, R4 v) => Lens' (t (v a)) (M24 a)
_m24 = column _xyzw . _xy

-- | Extract a 3x2 matrix from a matrix of higher dimensions by dropping excess
--  rows and columns.
_m32 :: (Vec t, R3 t, R2 v) => Lens' (t (v a)) (M32 a)
_m32 = column _xy . _xyz

-- | Extract a 3x3 matrix from a matrix of higher dimensions by dropping excess
--  rows and columns.
_m33 :: (Vec t, R3 t, R3 v) => Lens' (t (v a)) (M33 a)
_m33 = column _xyz . _xyz

-- | Extract a 3x4 matrix from a matrix of higher dimensions by dropping excess
--  rows and columns.
_m34 :: (Vec t, R3 t, R4 v) => Lens' (t (v a)) (M34 a)
_m34 = column _xyzw . _xyz

-- | Extract a 4x2 matrix from a matrix of higher dimensions by dropping excess
--  rows and columns.
_m42 :: (Vec t, R4 t, R2 v) => Lens' (t (v a)) (M42 a)
_m42 = column _xy . _xyzw

-- | Extract a 4x3 matrix from a matrix of higher dimensions by dropping excess
--  rows and columns.
_m43 :: (Vec t, R4 t, R3 v) => Lens' (t (v a)) (M43 a)
_m43 = column _xyz . _xyzw

-- | Extract a 4x4 matrix from a matrix of higher dimensions by dropping excess
--  rows and columns.
_m44 :: (Vec t, R4 t, R4 v) => Lens' (t (v a)) (M44 a)
_m44 = column _xyzw . _xyzw

-- | 2x2 matrix determinant.
--
--  >>> det22 (V2 (V2 a b) (V2 c d))
--  a * d - b * c
det22 :: Num a => M22 a -> a
det22 (V2 (V2 a b) (V2 c d)) = a * d - b * c
{-# INLINE det22 #-}

-- | 3x3 matrix determinant.
--
--  >>> det33 (V3 (V3 a b c) (V3 d e f) (V3 g h i))
--  a * (e * i - f * h) - d * (b * i - c * h) + g * (b * f - c * e)
det33 :: Num a => M33 a -> a
det33
  ( V3
      (V3 a b c)
      (V3 d e f)
      (V3 g h i)
    ) = a * (e * i - f * h) - d * (b * i - c * h) + g * (b * f - c * e)
{-# INLINE det33 #-}

-- | 4x4 matrix determinant.
det44 :: Num a => M44 a -> a
det44
  ( V4
      (V4 i00 i01 i02 i03)
      (V4 i10 i11 i12 i13)
      (V4 i20 i21 i22 i23)
      (V4 i30 i31 i32 i33)
    ) =
    let s0 = i00 * i11 - i10 * i01
        s1 = i00 * i12 - i10 * i02
        s2 = i00 * i13 - i10 * i03
        s3 = i01 * i12 - i11 * i02
        s4 = i01 * i13 - i11 * i03
        s5 = i02 * i13 - i12 * i03

        c5 = i22 * i33 - i32 * i23
        c4 = i21 * i33 - i31 * i23
        c3 = i21 * i32 - i31 * i22
        c2 = i20 * i33 - i30 * i23
        c1 = i20 * i32 - i30 * i22
        c0 = i20 * i31 - i30 * i21
     in s0 * c5 - s1 * c4 + s2 * c3 + s3 * c2 - s4 * c1 + s5 * c0
{-# INLINE det44 #-}

-- | 2x2 matrix inverse.
--
--  >>> inv22 $ V2 (V2 1 2) (V2 3 4)
--  V2 (V2 (-2.0) 1.0) (V2 1.5 (-0.5))
inv22 :: Fractional a => M22 a -> M22 a
inv22 m@(V2 (V2 a b) (V2 c d)) = (1 / det) *!! V2 (V2 d (-b)) (V2 (-c) a)
  where
    det = det22 m
{-# INLINE inv22 #-}

-- | 3x3 matrix inverse.
--
--  >>> inv33 $ V3 (V3 1 2 4) (V3 4 2 2) (V3 1 1 1)
--  V3 (V3 0.0 0.5 (-1.0)) (V3 (-0.5) (-0.75) 3.5) (V3 0.5 0.25 (-1.5))
inv33 :: Fractional a => M33 a -> M33 a
inv33
  m@( V3
        (V3 a b c)
        (V3 d e f)
        (V3 g h i)
      ) =
    (1 / det)
      *!! V3
        (V3 a' b' c')
        (V3 d' e' f')
        (V3 g' h' i')
    where
      a' = cofactor (e, f, h, i)
      b' = cofactor (c, b, i, h)
      c' = cofactor (b, c, e, f)
      d' = cofactor (f, d, i, g)
      e' = cofactor (a, c, g, i)
      f' = cofactor (c, a, f, d)
      g' = cofactor (d, e, g, h)
      h' = cofactor (b, a, h, g)
      i' = cofactor (a, b, d, e)
      cofactor (q, r, s, t) = det22 (V2 (V2 q r) (V2 s t))
      det = det33 m
{-# INLINE inv33 #-}

-- | 'transpose' is just an alias for 'distribute'
--
-- > transpose (V3 (V2 1 2) (V2 3 4) (V2 5 6))
-- V2 (V3 1 3 5) (V3 2 4 6)
transpose :: (Vec f, Vec g) => f (g a) -> g (f a)
transpose = sequenceA
{-# INLINE transpose #-}

-- | 4x4 matrix inverse.
inv44 :: Fractional a => M44 a -> M44 a
inv44
  ( V4
      (V4 i00 i01 i02 i03)
      (V4 i10 i11 i12 i13)
      (V4 i20 i21 i22 i23)
      (V4 i30 i31 i32 i33)
    ) =
    let s0 = i00 * i11 - i10 * i01
        s1 = i00 * i12 - i10 * i02
        s2 = i00 * i13 - i10 * i03
        s3 = i01 * i12 - i11 * i02
        s4 = i01 * i13 - i11 * i03
        s5 = i02 * i13 - i12 * i03
        c5 = i22 * i33 - i32 * i23
        c4 = i21 * i33 - i31 * i23
        c3 = i21 * i32 - i31 * i22
        c2 = i20 * i33 - i30 * i23
        c1 = i20 * i32 - i30 * i22
        c0 = i20 * i31 - i30 * i21
        det = s0 * c5 - s1 * c4 + s2 * c3 + s3 * c2 - s4 * c1 + s5 * c0
        invDet = recip det
     in invDet
          *!! V4
            ( V4
                (i11 * c5 - i12 * c4 + i13 * c3)
                (-i01 * c5 + i02 * c4 - i03 * c3)
                (i31 * s5 - i32 * s4 + i33 * s3)
                (-i21 * s5 + i22 * s4 - i23 * s3)
            )
            ( V4
                (-i10 * c5 + i12 * c2 - i13 * c1)
                (i00 * c5 - i02 * c2 + i03 * c1)
                (-i30 * s5 + i32 * s2 - i33 * s1)
                (i20 * s5 - i22 * s2 + i23 * s1)
            )
            ( V4
                (i10 * c4 - i11 * c2 + i13 * c0)
                (-i00 * c4 + i01 * c2 - i03 * c0)
                (i30 * s4 - i31 * s2 + i33 * s0)
                (-i20 * s4 + i21 * s2 - i23 * s0)
            )
            ( V4
                (-i10 * c3 + i11 * c1 - i12 * c0)
                (i00 * c3 - i01 * c1 + i02 * c0)
                (-i30 * s3 + i31 * s1 - i32 * s0)
                (i20 * s3 - i21 * s1 + i22 * s0)
            )
{-# INLINE inv44 #-}
