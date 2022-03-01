-- |
-- Functions for working with 3-dimensional homogeneous coordinates.
-- This includes common projection matrices: e.g. perspective/orthographic transformation
-- matrices.
--
-- Analytically derived inverses are also supplied, because they can be
-- much more accurate in practice than computing them through general
-- purpose means
--
-- Adapted from [Linear.Projection](https://hackage.haskell.org/package/linear-1.21.8/docs/Linear-Projection.html)
module Nonlinear.Projective.Hom3 where

import Nonlinear.Internal (Lens')
import Nonlinear.Matrix
import Nonlinear.Quaternion
import Nonlinear.V3
import Nonlinear.V4
import Nonlinear.Vector

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
-- y\/w, z\/w)@ where the projective, homogeneous, coordinate
-- @[x:y:z:w]@ is one of many associated with a single point @(x\/w,
-- y\/w, z\/w)@.
normalizePoint :: Fractional a => V4 a -> V3 a
normalizePoint (V4 a b c w) = (1 / w) *^ V3 a b c
{-# INLINE normalizePoint #-}

-- | Convert a 3x3 matrix to a 4x4 matrix extending it with 0's in the new row and column.
m33_to_m44 :: Num a => M33 a -> M44 a
m33_to_m44 (V3 r1 r2 r3) = V4 (vector r1) (vector r2) (vector r3) (point 0)

-- | Extract the translation vector (first three entries of the last
--  column) from a 3x4 or 4x4 matrix.
translation :: (Vec t, R3 t, R4 v) => Lens' (t (v a)) (V3 a)
translation = column _w . _xyz

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

{-# SPECIALIZE lookAt :: V3 Float -> V3 Float -> V3 Float -> M44 Float #-}
{-# SPECIALIZE lookAt :: V3 Double -> V3 Double -> V3 Double -> M44 Double #-}

-- | Build a look at view matrix
lookAt ::
  (Floating a) =>
  -- | Eye
  V3 a ->
  -- | Center
  V3 a ->
  -- | Up
  V3 a ->
  M44 a
lookAt eye center up =
  V4
    (snoc3 xa xd)
    (snoc3 ya yd)
    (snoc3 (-za) zd)
    (V4 0 0 0 1)
  where
    snoc3 (V3 a b c) = V4 a b c
    za = normalize $ center - eye
    xa = normalize $ cross za up
    ya = cross xa za
    xd = -dot xa eye
    yd = -dot ya eye
    zd = dot za eye

{-# SPECIALIZE perspective :: Float -> Float -> Float -> Float -> M44 Float #-}
{-# SPECIALIZE perspective :: Double -> Double -> Double -> Double -> M44 Double #-}

-- | Build a matrix for a symmetric perspective-view frustum
perspective ::
  Floating a =>
  -- | FOV (y direction, in radians)
  a ->
  -- | Aspect ratio
  a ->
  -- | Near plane
  a ->
  -- | Far plane
  a ->
  M44 a
perspective fovy aspect near far =
  V4
    (V4 x 0 0 0)
    (V4 0 y 0 0)
    (V4 0 0 z w)
    (V4 0 0 (-1) 0)
  where
    tanHalfFovy = tan $ fovy / 2
    x = 1 / (aspect * tanHalfFovy)
    y = 1 / tanHalfFovy
    fpn = far + near
    fmn = far - near
    oon = 0.5 / near
    oof = 0.5 / far
    -- z = 1 / (near/fpn - far/fpn) -- would be better by .5 bits
    z = -fpn / fmn
    w = 1 / (oof - oon) -- 13 bits error reduced to 0.17
    -- w = -(2 * far * near) / fmn

{-# SPECIALIZE inversePerspective :: Float -> Float -> Float -> Float -> M44 Float #-}
{-# SPECIALIZE inversePerspective :: Double -> Double -> Double -> Double -> M44 Double #-}

-- | Build an inverse perspective matrix
inversePerspective ::
  Floating a =>
  -- | FOV (y direction, in radians)
  a ->
  -- | Aspect ratio
  a ->
  -- | Near plane
  a ->
  -- | Far plane
  a ->
  M44 a
inversePerspective fovy aspect near far =
  V4
    (V4 a 0 0 0)
    (V4 0 b 0 0)
    (V4 0 0 0 (-1))
    (V4 0 0 c d)
  where
    tanHalfFovy = tan $ fovy / 2
    a = aspect * tanHalfFovy
    b = tanHalfFovy
    c = oon - oof
    d = oon + oof
    oon = 0.5 / near
    oof = 0.5 / far

{-# SPECIALIZE frustum :: Float -> Float -> Float -> Float -> Float -> Float -> M44 Float #-}
{-# SPECIALIZE frustum :: Double -> Double -> Double -> Double -> Double -> Double -> M44 Double #-}

-- | Build a perspective matrix per the classic @glFrustum@ arguments.
frustum ::
  Floating a =>
  -- | Left
  a ->
  -- | Right
  a ->
  -- | Bottom
  a ->
  -- | Top
  a ->
  -- | Near
  a ->
  -- | Far
  a ->
  M44 a
frustum l r b t n f =
  V4
    (V4 x 0 a 0)
    (V4 0 y e 0)
    (V4 0 0 c d)
    (V4 0 0 (-1) 0)
  where
    rml = r - l
    tmb = t - b
    fmn = f - n
    x = 2 * n / rml
    y = 2 * n / tmb
    a = (r + l) / rml
    e = (t + b) / tmb
    c = negate (f + n) / fmn
    d = (-2 * f * n) / fmn

{-# SPECIALIZE inverseFrustum :: Float -> Float -> Float -> Float -> Float -> Float -> M44 Float #-}
{-# SPECIALIZE inverseFrustum :: Double -> Double -> Double -> Double -> Double -> Double -> M44 Double #-}
inverseFrustum ::
  Floating a =>
  -- | Left
  a ->
  -- | Right
  a ->
  -- | Bottom
  a ->
  -- | Top
  a ->
  -- | Near
  a ->
  -- | Far
  a ->
  M44 a
inverseFrustum l r b t n f =
  V4
    (V4 rx 0 0 ax)
    (V4 0 ry 0 by)
    (V4 0 0 0 (-1))
    (V4 0 0 rd cd)
  where
    hrn = 0.5 / n
    hrnf = 0.5 / (n * f)
    rx = (r - l) * hrn
    ry = (t - b) * hrn
    ax = (r + l) * hrn
    by = (t + b) * hrn
    cd = (f + n) * hrnf
    rd = (n - f) * hrnf

{-# SPECIALIZE infinitePerspective :: Float -> Float -> Float -> M44 Float #-}
{-# SPECIALIZE infinitePerspective :: Double -> Double -> Double -> M44 Double #-}

-- | Build a matrix for a symmetric perspective-view frustum with a far plane at infinite
infinitePerspective ::
  Floating a =>
  -- | FOV (y direction, in radians)
  a ->
  -- | Aspect Ratio
  a ->
  -- | Near plane
  a ->
  M44 a
infinitePerspective fovy a n =
  V4
    (V4 x 0 0 0)
    (V4 0 y 0 0)
    (V4 0 0 (-1) w)
    (V4 0 0 (-1) 0)
  where
    t = n * tan (fovy / 2)
    b = -t
    l = b * a
    r = t * a
    x = (2 * n) / (r - l)
    y = (2 * n) / (t - b)
    w = -2 * n

{-# SPECIALIZE inverseInfinitePerspective :: Float -> Float -> Float -> M44 Float #-}
{-# SPECIALIZE inverseInfinitePerspective :: Double -> Double -> Double -> M44 Double #-}
inverseInfinitePerspective ::
  Floating a =>
  -- | FOV (y direction, in radians)
  a ->
  -- | Aspect Ratio
  a ->
  -- | Near plane
  a ->
  M44 a
inverseInfinitePerspective fovy a n =
  V4
    (V4 rx 0 0 0)
    (V4 0 ry 0 0)
    (V4 0 0 0 (-1))
    (V4 0 0 rw (-rw))
  where
    t = n * tan (fovy / 2)
    b = -t
    l = b * a
    r = t * a
    hrn = 0.5 / n
    rx = (r - l) * hrn
    ry = (t - b) * hrn
    rw = -hrn

{-# SPECIALIZE ortho :: Float -> Float -> Float -> Float -> Float -> Float -> M44 Float #-}
{-# SPECIALIZE ortho :: Double -> Double -> Double -> Double -> Double -> Double -> M44 Double #-}

-- | Build an orthographic perspective matrix from 6 clipping planes.
-- This matrix takes the region delimited by these planes and maps it
-- to normalized device coordinates between [-1,1]
--
-- This call is designed to mimic the parameters to the OpenGL @glOrtho@
-- call, so it has a slightly strange convention: Notably: the near and
-- far planes are negated.
--
-- Consequently:
--
-- @
-- 'ortho' l r b t n f !* 'V4' l b (-n) 1 = 'V4' (-1) (-1) (-1) 1
-- 'ortho' l r b t n f !* 'V4' r t (-f) 1 = 'V4' 1 1 1 1
-- @
--
-- Examples:
--
-- >>> ortho 1 2 3 4 5 6 !* V4 1 3 (-5) 1
-- V4 (-1.0) (-1.0) (-1.0) 1.0
--
-- >>> ortho 1 2 3 4 5 6 !* V4 2 4 (-6) 1
-- V4 1.0 1.0 1.0 1.0
ortho ::
  Fractional a =>
  -- | Left
  a ->
  -- | Right
  a ->
  -- | Bottom
  a ->
  -- | Top
  a ->
  -- | Near
  a ->
  -- | Far
  a ->
  M44 a
ortho l r b t n f =
  V4
    (V4 (-2 * x) 0 0 ((r + l) * x))
    (V4 0 (-2 * y) 0 ((t + b) * y))
    (V4 0 0 (2 * z) ((f + n) * z))
    (V4 0 0 0 1)
  where
    x = recip (l - r)
    y = recip (b - t)
    z = recip (n - f)

{-# SPECIALIZE inverseOrtho :: Float -> Float -> Float -> Float -> Float -> Float -> M44 Float #-}
{-# SPECIALIZE inverseOrtho :: Double -> Double -> Double -> Double -> Double -> Double -> M44 Double #-}

-- | Build an inverse orthographic perspective matrix from 6 clipping planes
inverseOrtho ::
  Fractional a =>
  -- | Left
  a ->
  -- | Right
  a ->
  -- | Bottom
  a ->
  -- | Top
  a ->
  -- | Near
  a ->
  -- | Far
  a ->
  M44 a
inverseOrtho l r b t n f =
  V4
    (V4 x 0 0 c)
    (V4 0 y 0 d)
    (V4 0 0 z e)
    (V4 0 0 0 1)
  where
    x = 0.5 * (r - l)
    y = 0.5 * (t - b)
    z = 0.5 * (n - f)
    c = 0.5 * (l + r)
    d = 0.5 * (b + t)
    e = -0.5 * (n + f)
