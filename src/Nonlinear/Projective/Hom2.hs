-- |
-- Functions for working with 2-dimensional homogeneous coordinates.
module Nonlinear.Projective.Hom2 where

import Nonlinear.Matrix
import Nonlinear.V2
import Nonlinear.V3
import Nonlinear.Vector ((*^))

-- | Convert a 2-dimensional affine vector into a 3-dimensional homogeneous vector,
-- i.e. sets the @w@ coordinate to 0.
vector :: Num a => V2 a -> V3 a
vector (V2 a b) = V3 a b 0
{-# INLINE vector #-}

-- | Convert a 2-dimensional affine point into a 3-dimensional homogeneous vector,
-- i.e. sets the @w@ coordinate to 1.
point :: Num a => V2 a -> V3 a
point (V2 a b) = V3 a b 1
{-# INLINE point #-}

-- | Convert 3-dimensional projective coordinates to a 2-dimensional
-- point. This operation may be denoted, @euclidean [x:y:w] = (x\/w,
-- y\/w)@ where the projective, homogeneous, coordinate
-- @[x:y:z]@ is one of many associated with a single point @(x\/w,
-- y\/w)@.
normalizePoint :: Fractional a => V3 a -> V2 a
normalizePoint (V3 a b w) = (1 / w) *^ V2 a b
{-# INLINE normalizePoint #-}

mkTransformation :: Num a => M22 a -> V2 a -> M33 a
mkTransformation (V2 r1 r2) (V2 tx ty) =
  V3 (snoc2 r1 tx) (snoc2 r2 ty) (V3 0 0 1)
  where
    snoc2 (V2 x y) = V3 x y
{-# INLINE mkTransformation #-}

-- | translate along two axes
translation :: Num a => V2 a -> M33 a
translation = mkTransformation identity

-- | rotate a radiant angle
rotateRad :: Floating a => a -> M33 a
rotateRad rad = mkTransformation rot 0
  where
    cosR = cos rad
    sinR = sin rad
    rot =
      V2
        (V2 cosR (negate sinR))
        (V2 sinR cosR)

-- | Convert a 2x2 matrix to a 3x3 matrix extending it with 0's in the new row and column.
m22_to_m33 :: Num a => M22 a -> M33 a
m22_to_m33 (V2 r1 r2) = V3 (vector r1) (vector r2) (point 0)
