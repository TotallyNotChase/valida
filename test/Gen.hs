{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Gen
    ( ValidationQ (..)
    ) where

import Test.SmallCheck.Series (Serial (..), cons1, (\/))
import Test.Tasty.QuickCheck  (Arbitrary (arbitrary, shrink), oneof)

import Valida (Validation (..))

newtype ValidationQ e a = ValidationQ (Validation e a)
  deriving (Show)

instance (Arbitrary e, Arbitrary a) => Arbitrary (ValidationQ e a) where
    arbitrary = ValidationQ <$> oneof [Failure <$> arbitrary, Success <$> arbitrary]
    shrink (ValidationQ (Failure e)) = ValidationQ . Failure <$> shrink e
    shrink (ValidationQ (Success a)) = ValidationQ . Success <$> shrink a

instance (Serial m e, Serial m a) => Serial m (ValidationQ e a) where
    series = ValidationQ <$> cons1 Failure \/ cons1 Success
