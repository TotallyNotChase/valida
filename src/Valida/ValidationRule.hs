module Valida.ValidationRule
    ( ValidationRule (..)
    , failureIf
    , failureIf'
    , failureUnless
    , failureUnless'
    , label
    , (<?>)
    ) where

import Data.List.NonEmpty (NonEmpty (..))

-- | The rule a `Validator` uses to run validation.
data ValidationRule e a
  -- | Builds a `ValidationRule` from a function to generate error and a validation predicate.
  = ValidationRule
  -- ^ Function taking the validation target and returning a value representing the error.
    (a -> e)
  -- ^ The validation predicate.
    (a -> Bool)

-- | Build a predicate that fails with given error if the given predicate succeeds.
failureIf :: (a -> Bool) -> e -> ValidationRule (NonEmpty e) a
failureIf predicate err = ValidationRule (const $ err :| []) $ not . predicate

-- | Like `failureIf` but uses Unit as the `ValidationRule` error type.
failureIf' :: (a -> Bool) -> ValidationRule () a
failureIf' = ValidationRule (const ()) . (not .)

-- | Build a predicate that fails with given error unless the given predicate succeeds.
failureUnless :: (a -> Bool) -> e -> ValidationRule (NonEmpty e) a
failureUnless predicate err = ValidationRule (const $ err :| []) predicate

-- | Like `failureUnless` but uses Unit as the `ValidationRule` error type.
failureUnless' :: (a -> Bool) -> ValidationRule () a
failureUnless' = ValidationRule (const ())

-- | Relabel a `ValidationRule` with a new error generator.
label :: (a -> e) -> ValidationRule x a -> ValidationRule e a
label errF (ValidationRule _ p) = ValidationRule errF p

-- | A synonym for `label` with its arguments flipped.
infix 6 <?>

(<?>) :: ValidationRule x a -> (a -> e) -> ValidationRule e a
(<?>) = flip label
