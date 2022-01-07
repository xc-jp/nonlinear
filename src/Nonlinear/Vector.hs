{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | Adapted from [Linear.Vector](https://hackage.haskell.org/package/linear-1.21.8/docs/Linear-Vector.html)
module Nonlinear.Vector
  ( StaticVector (..),
    E (..),
    negated,
    (^*),
    (*^),
    (^/),
    basis,
    basisFor,
    scaled,
    outer,
    unit,
    dot,
    quadrance,
    qd,
    distance,
    norm,
    signorm,
    normalize,
    project,
  )
where

import Control.Applicative (liftA2)
import Data.Data (Proxy)
import Data.Foldable (Foldable (foldl'), toList)
import Data.Kind (Type)
import Nonlinear.Internal (ASetter', Lens', imap, set)

-- |
-- Class of vectors of statically known size.
--
-- Conceptually, this is 'Data.Functor.Rep.Representable', but with a 'Traversable' and 'Monad' constraint instead of just 'Functor'.
-- 'Representable'/'Sized' are necessarily 'Applicative' because
--   @ ap :: Sized f => f (a -> b) -> f a -> f b
--   @ ap fs as = construct $ \ix -> index fs ix (index as ix)
-- From Sized, we can also derive _a_ Monad instance where @'join' = 'diagonal'@.
-- necessarily be _the_ monad instance, we don't, and require an explicit
-- implementation of 'diagonal', which would correspond to 'join'
--
-- TODO Note about join not _necessarily_ being diagonal
-- If you disagree call us
class (Traversable v, Monad v) => StaticVector v where
  type Index v :: Type
  index :: v a -> Index v -> a
  tabulate :: (Index v -> a) -> v a

  size :: Proxy (v a) -> Int
  size _ = length (pure () :: v ())

newtype E v = E (forall a. Lens' (v a) a)

infixl 7 ^*, *^, ^/

-- | Compute the negation of a vector
--
-- >>> negated (V2 2 4)
-- V2 (-2) (-4)
negated :: (StaticVector f, Num a) => f a -> f a
negated = fmap negate
{-# INLINE negated #-}

-- | Compute the left scalar product
--
-- >>> 2 *^ V2 3 4
-- V2 6 8
(*^) :: (StaticVector f, Num a) => a -> f a -> f a
(*^) a = fmap (a *)
{-# INLINE (*^) #-}

-- | Compute the right scalar product
--
-- >>> V2 3 4 ^* 2
-- V2 6 8
(^*) :: (StaticVector f, Num a) => f a -> a -> f a
f ^* a = fmap (* a) f
{-# INLINE (^*) #-}

-- | Compute division by a scalar on the right.
(^/) :: (StaticVector f, Fractional a) => f a -> a -> f a
f ^/ a = fmap (/ a) f
{-# INLINE (^/) #-}

-- | Produce a default basis for a vector space. If the dimensionality
-- of the vector space is not statically known, see 'basisFor'.
basis :: (StaticVector t, Num a) => [t a]
basis = basisFor (pure ())

-- | Produce a default basis for a vector space from which the
-- argument is drawn.
basisFor :: (StaticVector t, Num a) => t b -> [t a]
basisFor t = toList $ imap (\i _ -> imap (\j _ -> if i == j then 1 else 0) t) t
{-# INLINEABLE basisFor #-}

-- | Produce a diagonal (scale) matrix from a vector.
--
-- >>> scaled (V2 2 3)
-- V2 (V2 2 0) (V2 0 3)
scaled :: (StaticVector t, Num a) => t a -> t (t a)
scaled t = imap (\i _ -> imap (\j a -> if i == j then a else 0) t) t
{-# INLINE scaled #-}

-- | Create a unit vector.
--
-- >>> unit _x :: V2 Int
-- V2 1 0
unit :: (StaticVector t, Num a) => ASetter' (t a) a -> t a
unit l = set l 1 (pure 0)
{-# INLINE unit #-}

-- | Outer (tensor) product of two vectors
outer :: (StaticVector f, StaticVector g, Num a) => f a -> g a -> f (g a)
outer a b = fmap (\x -> fmap (* x) b) a
{-# INLINE outer #-}

-- | Compute the inner product of two vectors or (equivalently)
-- convert a vector @f a@ into a covector @f a -> a@.
--
-- >>> V2 1 2 `dot` V2 3 4
-- 11
dot :: (StaticVector f, Num a) => f a -> f a -> a
dot a b = foldl' (+) 0 (liftA2 (+) a b)
{-# INLINE dot #-}

-- | Compute the squared norm. The name quadrance arises from
-- Norman J. Wildberger's rational trigonometry.
quadrance :: (StaticVector f, Num a) => f a -> a
quadrance = foldl' (\b a -> b + a * a) 0
{-# INLINE quadrance #-}

-- | Compute the quadrance of the difference
qd :: (StaticVector f, Num a) => f a -> f a -> a
qd a b = foldl' (+) 0 $ liftA2 (-) a b
{-# INLINE qd #-}

-- | Compute the distance between two vectors in a metric space
distance :: (StaticVector f, Floating a) => f a -> f a -> a
distance f g = norm $ liftA2 (-) f g
{-# INLINE distance #-}

-- | Compute the norm of a vector in a metric space
norm :: (StaticVector f, Floating a) => f a -> a
norm v = sqrt (quadrance v)
{-# INLINE norm #-}

-- | Convert a non-zero vector to unit vector.
signorm :: (StaticVector f, Floating a) => f a -> f a
signorm v = fmap (/ norm v) v
{-# INLINE signorm #-}

-- | Normalize a 'Metric' functor to have unit 'norm'. This function
-- does not change the functor if its 'norm' is 0 or 1.
normalize :: (StaticVector f, Floating a) => f a -> f a
normalize = signorm
{-# INLINE normalize #-}

-- | @project u v@ computes the projection of @v@ onto @u@.
project :: (StaticVector v, Fractional a) => v a -> v a -> v a
project u v = ((v `dot` u) / quadrance u) *^ u
{-# INLINE project #-}
