{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Gen
    ( NonEmptyLQ
    , ValidationQ (..)
    ) where

import qualified Data.List.NonEmpty as NE

import           Test.SmallCheck.Series (Serial (..), cons1, (\/))
import           Test.Tasty.QuickCheck  (Arbitrary (arbitrary, shrink), oneof)
import qualified Test.Tasty.QuickCheck  as QC


import Valida (Validation (..))

-- | Wrapper around 'Validation' with 'Arbitrary' and 'Serial' instances.
newtype ValidationQ e a = ValidationQ (Validation e a)
  deriving (Show)

instance (Arbitrary e, Arbitrary a) => Arbitrary (ValidationQ e a) where
    arbitrary = ValidationQ <$> oneof [Failure <$> arbitrary, Success <$> arbitrary]
    shrink (ValidationQ (Failure e)) = ValidationQ . Failure <$> shrink e
    shrink (ValidationQ (Success a)) = ValidationQ . Success <$> shrink a

instance (Serial m e, Serial m a) => Serial m (ValidationQ e a) where
    series = ValidationQ <$> cons1 Failure \/ cons1 Success

-- | Type unifying the separate `NonEmpty` impls on quickcheck and smallcheck.
data NonEmptyLQ a = NonEmptyListQ (QC.NonEmptyList a) | NonEmptyQ (NE.NonEmpty a)
  deriving (Show)

instance Arbitrary a => Arbitrary (NonEmptyLQ a) where
    arbitrary = NonEmptyListQ <$> arbitrary
    shrink (NonEmptyListQ l) = NonEmptyListQ <$> shrink l
    shrink _                 = error "Should not try to shrink NonEmptyQ"

instance Serial m a => Serial m (NonEmptyLQ a) where
    series = cons1 NonEmptyQ

instance Functor NonEmptyLQ where
    fmap f (NonEmptyListQ l) = NonEmptyListQ $ f <$> l
    fmap f (NonEmptyQ l)     = NonEmptyQ $ f <$> l

instance Foldable NonEmptyLQ where
    foldMap f (NonEmptyListQ l) = foldMap f $ QC.getNonEmpty l
    foldMap f (NonEmptyQ l)     = foldMap f l
