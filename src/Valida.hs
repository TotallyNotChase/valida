{-# LANGUAGE Safe #-}

module Valida
    ( Selector
      -- * Primary data types
    , Validation (..)
    , ValidationRule
    , Validator (runValidator)
      -- * Functions for building Valida data types
    , validate
    , verify
    , vrule
    , (-?>)
      -- * Reassigning corresponding error to 'ValidationRule'
    , label
    , (<?>)
      -- | Re-exports of "Valida.Combinators"
    , module Valida.Combinators
    , module Valida.ValidationUtils
    ) where

import Control.Applicative (Applicative (liftA2))
import Data.Bifunctor      (Bifunctor (first))

import Valida.Combinators
import Valida.Validation      (Validation (..))
import Valida.ValidationRule  (ValidationRule (..), vrule)
import Valida.ValidationUtils
import Valida.Validator       (Selector, Validator (..))

infix 5 `verify`

{- | Build a validator from a 'ValidationRule' and a 'Selector'.

The 'Validator` first runs given __selector__ on its input to obtain the validation target. Then, it runs the
'ValidationRule' on the target.

If validation is successful, the validation target is put into the 'Validation' result.
-}
verify :: ValidationRule e b -> Selector a b -> Validator e a b
verify (ValidationRule rule) selector = Validator $ liftA2 (<$) selector (rule . selector)

-- | A synonym for 'verify' with its arguments flipped.
infix 5 -?>

(-?>) :: Selector a b -> ValidationRule e b -> Validator e a b
(-?>) = flip verify

---------------------------------------------------------------------
-- Reassigning corresponding error to 'ValidationRule'.
---------------------------------------------------------------------

{- | Relabel a 'ValidationRule' with a different error, obtained from an "error generator".

An "error generator" is a function that takes the validation target, that has failed validation, and returns a value
representing error.

Many combinators, like 'failureIf' and 'failureUnless', simply return the given error value
within /NonEmpty/ upon failure. You can use 'label' to override this return value.
-}
label :: (a -> e) -> ValidationRule x a -> ValidationRule e a
label errF (ValidationRule rule) = vrule $ (first . const . errF) <*> rule

-- | A synonym for 'label' with its arguments flipped.
infix 6 <?>

(<?>) :: ValidationRule x a -> (a -> e) -> ValidationRule e a
(<?>) = flip label

{- | Build a basic validator from a 'ValidationRule'.

The 'Validator' runs the rule on its input. If validation is successful, the input is put into the 'Validation'
result.

prop> validate rule = verify rule id
-}
validate :: ValidationRule e a -> Validator e a a
validate = (-?>) id
