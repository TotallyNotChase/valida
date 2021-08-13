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

{- |
* '(<>)' creates a new `ValidationRule` that only succeeds when both given rule succeed.
Otherwise left-most failure is returned.
-}
instance Semigroup (ValidationRule e a) where
    ValidationRule rl1 <> ValidationRule rl2 = ValidationRule
        $ \x -> case (rl1 x, rl2 x) of
            (f@(Failure _), _) -> f
            (_, f@(Failure _)) -> f
            _                  -> Success ()

{- |
* 'mempty' is a 'ValidationRule' that always succeeds.
-}
instance Monoid (ValidationRule e a) where
    mempty = ValidationRule $ const $ Success ()

-- | Low level function to manually build a `ValidationRule`. You should use the combinators instead.
vrule :: (a -> Validation e ()) -> ValidationRule e a
vrule = ValidationRule
