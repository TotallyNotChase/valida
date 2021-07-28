module Valida.ValidationRule
    ( ValidationRule (..)
    , vrule
    ) where

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
    (a -> Validation e ())

-- | Low level function to manually build a `ValidationRule`. You should use the combinators instead.
vrule :: (a -> Validation e ()) -> ValidationRule e a
vrule = ValidationRule
