{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}

-- | Reimplementations of stuff otherwise found in other libraries
--
-- "A little copying is better than a little dependency"
module Nonlinear.Internal
  ( imap,
    Lens',
    set,
    view,
    lens,
    ASetter',
  )
where

import Control.Applicative (Const (Const, getConst))
import Control.Monad (ap)
import Data.Functor.Identity (Identity (runIdentity))
import Data.Traversable (for)

{-# INLINE imap #-}
imap :: Traversable t => (Int -> a -> b) -> (t a -> t b)
imap f t = evalTally . for t $ \a -> flip f a <$> click

-- | Equivalent to State Int
newtype Tally a = Tally {unTally :: Int -> (Int, a)}
  deriving (Functor)

instance Applicative Tally where
  {-# INLINE pure #-}
  pure a = Tally $ \i -> (i, a)
  {-# INLINE (<*>) #-}
  (<*>) = ap

instance Monad Tally where
  {-# INLINE (>>=) #-}
  Tally sa >>= asb = Tally $ \i -> let (i', a) = sa i in unTally (asb a) i'

{-# INLINE click #-}
click :: Tally Int
click = Tally $ \i -> (i + 1, i)

{-# INLINE evalTally #-}
evalTally :: Tally a -> a
evalTally = snd . flip unTally 0

type Lens' s a = forall m. Functor m => (a -> m a) -> (s -> m s)

{-# INLINE set #-}
set :: ASetter' s a -> a -> s -> s
set l a s = runIdentity $ l (const $ pure a) s

{-# INLINE view #-}
view :: Lens' s a -> s -> a
view l s = getConst $ l Const s

{-# INLINE lens #-}
lens :: (s -> a) -> (s -> a -> s) -> Lens' s a
lens sa sab l s = sab s <$> l (sa s)

type ASetter' s a = (a -> Identity a) -> (s -> Identity s)
