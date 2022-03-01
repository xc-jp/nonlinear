{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}

import Data.Coerce (coerce)
import Data.Functor.Identity (Identity (Identity), runIdentity)
import Data.Ratio
import Nonlinear
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

instance Arbitrary a => Arbitrary (V4 a) where arbitrary = V4 <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary a => Arbitrary (V3 a) where arbitrary = V3 <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary a => Arbitrary (V2 a) where arbitrary = V2 <$> arbitrary <*> arbitrary

instance Arbitrary a => Arbitrary (V1 a) where arbitrary = V1 <$> arbitrary

data VSome f a
  = VS1 (f (V1 a))
  | VS2 (f (V2 a))
  | VS3 (f (V3 a))
  | VS4 (f (V4 a))
  deriving (Functor, Foldable, Traversable)

deriving instance
  ( Eq a,
    forall b. Eq b => Eq (f b)
  ) =>
  Eq (VSome f a)

deriving instance
  ( Show a,
    forall b. Show b => Show (f b)
  ) =>
  Show (VSome f a)

instance (Arbitrary a, forall a. Arbitrary a => Arbitrary (f a)) => Arbitrary (VSome f a) where
  arbitrary = oneof [VS1 <$> arbitrary, VS2 <$> arbitrary, VS3 <$> arbitrary, VS4 <$> arbitrary]

withVSome ::
  ( forall v.
    (forall a. Eq a => Eq (v a), Vec v, Monad v, Traversable v, R1 v) =>
    f (v a) ->
    r
  ) ->
  (VSome f a -> r)
withVSome f (VS1 v) = f v
withVSome f (VS2 v) = f v
withVSome f (VS3 v) = f v
withVSome f (VS4 v) = f v

prop_transpose_involutive :: VSome (VSome Identity) Int -> Bool
prop_transpose_involutive = withVSome $ withVSome $ \(Identity mat) -> sequenceA (sequenceA mat) == mat

prop_column_id_is_id :: VSome (VSome Identity) Int -> Bool
prop_column_id_is_id = withVSome $ withVSome $ \(Identity mat) -> over (column id) id mat == mat

prop_column_x_id_is_id :: VSome (VSome Identity) Int -> Bool
prop_column_x_id_is_id = withVSome $ withVSome $ \(Identity mat) -> over (column _x) id mat == mat

over :: forall s t a b. ((a -> Identity b) -> (s -> Identity t)) -> (a -> b) -> (s -> t)
over = coerce

prop_diagonal_scaled_identity :: VSome Identity Int -> Bool
prop_diagonal_scaled_identity = withVSome $ \(Identity vec) -> diagonal (scaled vec) == vec

prop_project_self_identity :: VSome Identity (Ratio Int) -> Bool
prop_project_self_identity = withVSome $ \(Identity vec) -> project vec vec == vec

prop_normalize_idempotent :: VSome Identity Double -> Property
prop_normalize_idempotent = withVSome $ \(Identity vec) ->
  not (all (== 0) vec)
    ==> fmap round (normalize (normalize vec)) == fmap round (normalize vec)

main :: IO ()
main = hspec $ do
  describe "distribute" $ do
    prop "transpose is involutive" prop_transpose_involutive
    prop "(diagonal . scaled) is identity" prop_transpose_involutive
    prop "(project x x) is identity" prop_transpose_involutive
    prop "normalize is idempotent" prop_normalize_idempotent
    prop "over (column id) id is identity " prop_column_id_is_id
    prop "over (column _x) id is identity " prop_column_x_id_is_id
