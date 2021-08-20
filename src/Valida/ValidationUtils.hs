{-# LANGUAGE Safe #-}

module Valida.ValidationUtils
    ( -- * Transformations between 'Either' and 'Validation'
      fromEither
    , toEither
      -- * Utilities for working with 'Validation'
    , validation
    , validationConst
    , fromSuccess
    , fromFailure
    , isSuccess
    , isFailure
    , successes
    , failures
    , partitionValidations
    ) where

import Valida.Validation (Validation (..), validation, validationConst)

{- | Convert a 'Validation' to an 'Either'.

Given, __Validation a b__-

  * __Failure a__ is converted to __Left a__.
  * __Success b__ is converted to __Right b__.

==== __Examples__

>>> toEither (Success 'c' :: Validation String Char)
Right 'c'
>>> toEither (Failure 42 :: Validation Int Char)
Left 42
-}
toEither :: Validation a b -> Either a b
toEither = validation Left Right

{- | Convert a 'Either' to an 'Validation'.

Given, __Either e a__-

  * __Left e__ is converted to __Failure e__.
  * __Right a__ is converted to __Success a__.

==== __Examples__

>>> fromEither (Right 'c' :: Either String Char)
Success 'c'
>>> fromEither (Left 42 :: Either Int Char)
Failure 42
-}
fromEither :: Either e a -> Validation e a
fromEither = either Failure Success

-- | Return True if the given value is a 'Failure'-value, False otherwise.
isFailure :: Validation e a -> Bool
isFailure (Failure _) = True
isFailure _           = False

-- | Return True if the given value is a 'Success'-value, False otherwise.
isSuccess :: Validation e a -> Bool
isSuccess (Success _) = True
isSuccess _           = False

{- | Return the contents of a 'Failure'-value or a default value otherwise.

==== __Examples__

>>> fromFailure 0 (Success 48 :: Validation Int Int)
0
>>> fromFailure 0 (Failure 27 :: Validation Int Int)
27
-}
fromFailure :: e -> Validation e a -> e
fromFailure _ (Failure e) = e
fromFailure e _           = e

{- | Return the contents of a 'Success'-value or a default value otherwise.

==== __Examples__

>>> fromSuccess 0 (Success 48 :: Validation Int Int)
48
>>> fromSuccess 0 (Failure 27 :: Validation Int Int)
0
-}
fromSuccess :: a -> Validation e a -> a
fromSuccess _ (Success a) = a
fromSuccess a _           = a

{- | Extracts from a list of 'Validation' all the 'Failure' values, in order.

==== __Examples__

>>> failures [Success 48, Failure "err1", Failure "err2", Success 2, Failure "err3"]
["err1","err2","err3"]
>>> failures ([Success 1, Success 2, Success 3] :: [Validation String Int])
[]
-}
failures :: [Validation e a] -> [e]
failures xs = [e | Failure e <- xs]
{-# INLINABLE failures #-}

{- | Extracts from a list of 'Validation' all the Success elements, in order.

==== __Examples__

>>> successes [Success 1, Failure "err1", Failure "err2", Success 2, Failure "err3"]
[1,2]
>>> successes ([Failure "err1", Failure "err2", Failure "err3"] :: [Validation String Int])
[]
-}
successes :: [Validation e a] -> [a]
successes xs = [a | Success a <- xs]
{-# INLINABLE successes #-}

{- | Partitions a list of Either into two lists.

All the Left elements are extracted, in order, to the first component of the output.
Similarly the Right elements are extracted to the second component of the output.

@partitionValidations xs = ('failures' xs, 'successes' xs)@

==== __Examples__

>>> partitionValidations [Success 1, Failure "err1", Failure "err2", Success 2, Failure "err3"]
(["err1","err2","err3"],[1,2])
-}
partitionValidations :: [Validation e a] -> ([e], [a])
partitionValidations = foldr (validation failure success) ([],[])
  where
    failure a ~(l, r) = (a:l, r)
    success a ~(l, r) = (l, a:r)
