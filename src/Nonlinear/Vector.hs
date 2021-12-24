{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Copyright   :  (C) 2012-2015 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- Operations on free vector spaces.
module Nonlinear.Vector
  ( negated,
    (^*),
    (*^),
    (^/),
    basis,
    basisFor,
    scaled,
    outer,
    unit,
  )
where

import Control.Monad.Trans.State.Strict
import Data.Foldable (toList)
import Data.Traversable (for)
import Lens.Micro

infixl 7 ^*, *^, ^/

-- | Compute the negation of a vector
--
-- >>> negated (V2 2 4)
-- V2 (-2) (-4)
negated :: (Functor f, Num a) => f a -> f a
negated = fmap negate
{-# INLINE negated #-}

-- | Compute the left scalar product
--
-- >>> 2 *^ V2 3 4
-- V2 6 8
(*^) :: (Functor f, Num a) => a -> f a -> f a
(*^) a = fmap (a *)
{-# INLINE (*^) #-}

-- | Compute the right scalar product
--
-- >>> V2 3 4 ^* 2
-- V2 6 8
(^*) :: (Functor f, Num a) => f a -> a -> f a
f ^* a = fmap (* a) f
{-# INLINE (^*) #-}

-- | Compute division by a scalar on the right.
(^/) :: (Functor f, Fractional a) => f a -> a -> f a
f ^/ a = fmap (/ a) f
{-# INLINE (^/) #-}

-- | Produce a default basis for a vector space. If the dimensionality
-- of the vector space is not statically known, see 'basisFor'.
basis :: (Applicative t, Traversable t, Num a) => [t a]
basis = basisFor (pure ())

{-# INLINE imap #-}
imap :: Traversable t => (Int -> a -> b) -> (t a -> t b)
imap f t = flip evalState 0 $
  for t $ \a -> do
    n <- get
    put (n + 1)
    pure $ f n a

-- | Produce a default basis for a vector space from which the
-- argument is drawn.
basisFor :: (Traversable t, Num a) => t b -> [t a]
basisFor t = toList $ imap (\i _ -> imap (\j _ -> if i == j then 1 else 0) t) t
{-# INLINEABLE basisFor #-}

-- | Produce a diagonal (scale) matrix from a vector.
--
-- >>> scaled (V2 2 3)
-- V2 (V2 2 0) (V2 0 3)
scaled :: (Traversable t, Num a) => t a -> t (t a)
scaled t = imap (\i _ -> imap (\j a -> if i == j then a else 0) t) t
{-# INLINE scaled #-}

-- | Create a unit vector.
--
-- >>> unit _x :: V2 Int
-- V2 1 0
unit :: (Applicative t, Num a) => ASetter' (t a) a -> t a
unit l = set l 1 (pure 0)

-- | Outer (tensor) product of two vectors
outer :: (Functor f, Functor g, Num a) => f a -> g a -> f (g a)
outer a b = fmap (\x -> fmap (* x) b) a