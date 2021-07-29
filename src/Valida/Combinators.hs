module Valida.Combinators
    ( -- * Primitive 'NonEmpty' combinators
      failureIf
    , failureUnless
      -- * Primitive /Unit/ combinators
    , failureIf'
    , failureUnless'
      -- * Common derivates of primitive 'NonEmpty' combinators
    , atleastContains
    , maxLengthOf
    , maxValueOf
    , minLengthOf
    , minValueOf
    , mustBe
    , mustContain
    , onlyContains
      -- * Common derivates of primitive /Unit/ combinators
    , atleastContains'
    , maxLengthOf'
    , maxValueOf'
    , minLengthOf'
    , minValueOf'
    , mustBe'
    , mustContain'
    , onlyContains'
      -- * Combining 'ValidationRule's
    , andAlso
    , orElse
    , satisfyAll
    , satisfyAny
      -- * Reassigning corresponding error to 'ValidationRule'.
    , label
    , (<?>)
    ) where

import Data.Foldable      (toList)
import Data.List          (foldl1')
import Data.List.NonEmpty (NonEmpty)

import Valida.Utils          (singleton)
import Valida.Validation     (Validation (..))
import Valida.ValidationRule (ValidationRule (..), vrule)

---------------------------------------------------------------------
-- Primitive 'NonEmpty' combinators
---------------------------------------------------------------------

-- | Build a rule that /fails/ with given error __if the given rule succeeds__.
failureIf :: (a -> Bool) -> e -> ValidationRule (NonEmpty e) a
failureIf predc = predToRule (not . predc) . singleton

-- | Build a rule that /fails/ with given error __unless the given rule succeeds__.
failureUnless :: (a -> Bool) -> e -> ValidationRule (NonEmpty e) a
failureUnless predc = predToRule predc . singleton

---------------------------------------------------------------------
-- Primitive /Unit/ combinators
---------------------------------------------------------------------

-- | Like 'failureIf' but uses /Unit/ as the 'ValidationRule' error type.
failureIf' :: (a -> Bool) -> ValidationRule () a
failureIf' = flip predToRule () . (not .)

-- | Like 'failureUnless' but uses /Unit/ as the 'ValidationRule' error type.
failureUnless' :: (a -> Bool) -> ValidationRule () a
failureUnless' = flip predToRule ()

---------------------------------------------------------------------
-- Common derivates of primitive 'NonEmpty' combinators
---------------------------------------------------------------------

{- | Build a equality rule.

prop> mustBe x = failureUnless (==x)
-}
mustBe :: Eq a => a -> e -> ValidationRule (NonEmpty e) a
mustBe x = failureUnless (==x)

{- | Build a minimum length rule.

prop> minLengthOf x = failureUnless ((>=n) . length)
-}
minLengthOf :: Foldable t => Int -> e -> ValidationRule (NonEmpty e) (t a)
minLengthOf n = failureUnless ((>=n) . length)

{- | Build a maximum length rule.

prop> maxLengthOf n = failureUnless ((<=n) . length)
-}
maxLengthOf :: Foldable t => Int -> e -> ValidationRule (NonEmpty e) (t a)
maxLengthOf n = failureUnless ((<=n) . length)

{- | Build a minimum value rule.

prop> minValueOf x = failureUnless (>=x)
-}
minValueOf :: Ord a => a -> e -> ValidationRule (NonEmpty e) a
minValueOf x = failureUnless (>=x)

{- | Build a maximum value rule.

prop> maxValueOf x = failureUnless (<=x)
-}
maxValueOf :: Ord a => a -> e -> ValidationRule (NonEmpty e) a
maxValueOf x = failureUnless (<=x)

{- | Build an 'all' rule.

prop> onlyContains x = failureUnless (all x)
-}
onlyContains :: Foldable t => (a -> Bool) -> e -> ValidationRule (NonEmpty e) (t a)
onlyContains x = failureUnless (all x)

{- | Build an 'any' rule.

prop> atleastContains x = failureUnless (any x)
-}
atleastContains :: Foldable t => (a -> Bool) -> e -> ValidationRule (NonEmpty e) (t a)
atleastContains x = failureUnless (any x)

{- | Build an 'elem' rule.

prop> mustContain x = atleastContains (==x)

prop> mustContain x = failureUnless (elem x)
-}
mustContain :: (Foldable t, Eq a) => a -> e -> ValidationRule (NonEmpty e) (t a)
mustContain x = atleastContains (==x)

---------------------------------------------------------------------
-- Common derivates of primitive /Unit/ combinators
---------------------------------------------------------------------

-- | Like 'mustBe' but uses /Unit/ as the 'ValidationRule' error type.
mustBe' :: Eq a => a -> ValidationRule () a
mustBe' x = failureUnless' (==x)

-- | Like 'minLengthOf' but uses /Unit/ as the 'ValidationRule' error type.
minLengthOf' :: Foldable t => Int -> ValidationRule () (t a)
minLengthOf' n = failureUnless' ((>=n) . length)

-- | Like 'maxLengthOf' but uses /Unit/ as the 'ValidationRule' error type.
maxLengthOf' :: Foldable t => Int -> ValidationRule () (t a)
maxLengthOf' n = failureUnless' ((<=n) . length)

-- | Like 'minValueOf' but uses /Unit/ as the 'ValidationRule' error type.
minValueOf' :: Ord a => a -> ValidationRule () a
minValueOf' x = failureUnless' (>=x)

-- | Like 'maxValueOf' but uses /Unit/ as the 'ValidationRule' error type.
maxValueOf' :: Ord a => a -> ValidationRule () a
maxValueOf' x = failureUnless' (<=x)

-- | Like 'onlyContains' but uses /Unit/ as the 'ValidationRule' error type.
onlyContains' :: Foldable t => (a -> Bool) -> ValidationRule () (t a)
onlyContains' x = failureUnless' (all x)

-- | Like 'atleastContains' but uses /Unit/ as the 'ValidationRule' error type.
atleastContains' :: Foldable t => (a -> Bool) -> ValidationRule () (t a)
atleastContains' x = failureUnless' (any x)

-- | Like 'mustContain' but uses /Unit/ as the 'ValidationRule' error type.
mustContain' :: (Foldable t, Eq a) => a -> ValidationRule () (t a)
mustContain' x = failureUnless' (elem x)

---------------------------------------------------------------------
-- Combining 'ValidationRule's
---------------------------------------------------------------------

orElse :: Semigroup e => ValidationRule e a -> ValidationRule e a -> ValidationRule e a
orElse (ValidationRule rule1) (ValidationRule rule2) = vrule $ (<>) <$> rule1 <*> rule2

andAlso :: ValidationRule e a -> ValidationRule e a -> ValidationRule e a
andAlso vrule1 vrule2 = vrule1 <> vrule2

satisfyAny :: (Foldable t, Semigroup e) => t (ValidationRule e a) -> ValidationRule e a
satisfyAny = foldl1' orElse . toList

satisfyAll :: Foldable t => t (ValidationRule e a) -> ValidationRule e a
satisfyAll = foldl1' andAlso . toList

---------------------------------------------------------------------
-- Reassigning corresponding error to 'ValidationRule'.
---------------------------------------------------------------------

{- | Relabel a 'ValidationRule' with a different error, obtained from an "error generator".

An "error generator" is a function that takes the validation target, that has failed validation, and returns a value
representing error.

Many combinators, like 'failureIf' and 'failureUnless', simply return the given error value
within 'NonEmpty' upon failure. You can use 'label' to override this return value.
-}
label :: (a -> e) -> ValidationRule x a -> ValidationRule e a
label errF (ValidationRule rule) = vrule $ \x -> case rule x of
    Failure _ -> Failure (errF x)
    _         -> Success ()

-- | A synonym for 'label' with its arguments flipped.
infix 6 <?>

(<?>) :: ValidationRule x a -> (a -> e) -> ValidationRule e a
(<?>) = flip label

-- | Utility to convert a regular predicate function to a 'ValidationRule'. __INTERNAL__
predToRule :: (a -> Bool) -> e -> ValidationRule e a
predToRule predc err = vrule $ \x -> if predc x
    then Success ()
    else Failure err
