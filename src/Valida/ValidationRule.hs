module Valida.ValidationRule
    ( ValidationRule (..)
    , failureIf
    , failureIf'
    , failureUnless
    , failureUnless'
    , label
    , vrule
    , (<?>)
    ) where

import Data.List.NonEmpty (NonEmpty)

import Valida.Utils      (singleton)
import Valida.Validation (Validation (..))

{- | The rule a Validator uses to run validation.

Contains a function that accepts the target type and returns a `Validation` result.

The type- __ValidationRule (NonEmpty String) Int__, designates a rule that verifies the validity of an __Int__, and
uses a value of type __NonEmpty String__ to represent error, in case of failure.
-}
newtype ValidationRule e a
  -- | Builds a 'ValidationRule' from a function to generate error and a validation predicate.
  = ValidationRule
  -- ^ The validation predicate.
    (a -> Validation e a)

-- | Build a predicate that /fails/ with given error __if the given predicate succeeds__.
failureIf :: (a -> Bool) -> e -> ValidationRule (NonEmpty e) a
failureIf predc = predToRule (not . predc) . singleton

-- | Like 'failureIf' but uses 'Data.Unit' as the 'ValidationRule' error type.
failureIf' :: (a -> Bool) -> ValidationRule () a
failureIf' = flip predToRule () . (not .)

-- | Build a predicate that /fails/ with given error __unless the given predicate succeeds__.
failureUnless :: (a -> Bool) -> e -> ValidationRule (NonEmpty e) a
failureUnless predc = predToRule predc . singleton

-- | Like 'failureUnless' but uses /Unit/ as the 'ValidationRule' error type.
failureUnless' :: (a -> Bool) -> ValidationRule () a
failureUnless' = flip predToRule ()

-- | Relabel a 'ValidationRule' with a new error generator.
label :: (a -> e) -> ValidationRule x a -> ValidationRule e a
label errF (ValidationRule rule) = ValidationRule $ \x -> case rule x of
    Success a -> Success a
    Failure _ -> Failure (errF x)

-- | A synonym for 'label' with its arguments flipped.
infix 6 <?>

(<?>) :: ValidationRule x a -> (a -> e) -> ValidationRule e a
(<?>) = flip label

-- | Low level function to manually build a `ValidationRule`. You should use the combinators instead.
vrule :: (a -> Validation e a) -> ValidationRule e a
vrule = ValidationRule

predToRule :: (a -> Bool) -> e -> ValidationRule e a
predToRule predc err = ValidationRule $ \x -> if predc x
    then Success x
    else Failure err
