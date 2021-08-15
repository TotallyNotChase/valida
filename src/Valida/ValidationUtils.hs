{-# LANGUAGE Safe #-}

module Valida.ValidationUtils
    ( -- * Transformations between 'Either' and 'Validation'
      fromEither
    , toEither
      -- * Utilities for working with 'Validation'
    , failures
    , fromFailure
    , fromSuccess
    , isFailure
    , isSuccess
    , partitionValidations
    , successes
    , validation
    , validationConst
    ) where

import Valida.Validation (Validation (..), validation, validationConst)

{- | Convert a 'Validation' to an 'Either'.

Given, __Validation a b__-

  * __Failure a__ is converted to __Left a__.
  * __Success b__ is converted to __Right b__.
-}
toEither :: Validation a b -> Either a b
toEither = validation Left Right

{- | Convert a 'Either' to an 'Validation'.

Given, __Either e a__-

  * __Left e__ is converted to __Failure e__.
  * __Right a__ is converted to __Success a__.
-}
fromEither :: Either e a -> Validation e a
fromEither = either Failure Success

-- | Return True if the given value is a Failure-value, False otherwise.
isFailure :: Validation e a -> Bool
isFailure (Failure _) = True
isFailure _           = False

-- | Return True if the given value is a Success-value, False otherwise.
isSuccess :: Validation e a -> Bool
isSuccess (Success _) = True
isSuccess _           = False

-- | Return the contents of a Failure-value or a default value otherwise.
fromFailure :: e -> Validation e a -> e
fromFailure _ (Failure e) = e
fromFailure e _           = e

-- | Return the contents of a Success-value or a default value otherwise.
fromSuccess :: a -> Validation e a -> a
fromSuccess _ (Success a) = a
fromSuccess a _           = a

-- | Extracts from a list of 'Validation' all the Failure elements, in order.
failures :: [Validation e a] -> [e]
failures xs = [e | Failure e <- xs]
{-# INLINABLE failures #-}

-- | Extracts from a list of 'Validation' all the Success elements, in order.
successes :: [Validation e a] -> [a]
successes xs = [a | Success a <- xs]
{-# INLINABLE successes #-}

{- | Partitions a list of Either into two lists.

All the Left elements are extracted, in order, to the first component of the output.
Similarly the Right elements are extracted to the second component of the output.

prop> partitionValidations xs = (failures xs, successes xs)
-}
partitionValidations :: [Validation e a] -> ([e], [a])
partitionValidations = foldr (validation failure success) ([],[])
 where
  failure a ~(l, r) = (a:l, r)
  success a ~(l, r) = (l, a:r)
