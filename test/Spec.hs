{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}

import Data.Functor.Identity (Identity, runIdentity)
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
  arbitrary =
    oneof
      [ VS1 <$> arbitrary,
        VS2 <$> arbitrary,
        VS3 <$> arbitrary,
        VS4 <$> arbitrary
      ]

withVSome :: Eq a => (forall v. (Eq (v a), Applicative v, Traversable v) => f (v a) -> r) -> (VSome f a -> r)
withVSome f (VS1 v) = f v
withVSome f (VS2 v) = f v
withVSome f (VS3 v) = f v
withVSome f (VS4 v) = f v

prop_transpose_involutive :: VSome (VSome Identity) Int -> Bool
prop_transpose_involutive = withVSome (withVSome $ (\mat -> sequenceA (sequenceA mat) == mat) . runIdentity)

main :: IO ()
main = hspec $ do
  describe "distribute" $ do
    prop "transpose involutive" prop_transpose_involutive
