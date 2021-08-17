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
      -- * Reassigning errors
    , label
    , labelV
    , (<?>)
    , (<??>)
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

{- | Relabel a 'ValidationRule' with a different error.

Many combinators, like 'failureIf' and 'failureUnless', simply return the given error value
within /NonEmpty/ upon failure. You can use 'label' to override this return value.
-}
label :: e -> ValidationRule x a -> ValidationRule e a
label err (ValidationRule rule) = vrule $ first (const err) . rule

-- | A synonym for 'label' with its arguments flipped.
infix 6 <?>

(<?>) :: ValidationRule x a -> e -> ValidationRule e a
(<?>) = flip label

---------------------------------------------------------------------
-- Reassigning corresponding error to 'Validator'.
---------------------------------------------------------------------

-- | Relabel a 'Validator' with a different error.
labelV :: e -> Validator x inp a -> Validator e inp a
labelV err (Validator v) = Validator $ first (const err) . v

-- | A synonym for 'labelV' with its arguments flipped.
infix 6 <??>

(<??>) :: Validator x inp a -> e -> Validator e inp a
(<??>) = flip labelV

{- | Build a basic validator from a 'ValidationRule'.

The 'Validator' runs the rule on its input. If validation is successful, the input is put into the 'Validation'
result.

prop> validate rule = verify rule id
-}
validate :: ValidationRule e a -> Validator e a a
validate = (-?>) id
