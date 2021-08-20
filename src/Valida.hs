{-# LANGUAGE Safe #-}

{- |
Module      : Valida
Description : Simple applicative validation for product types, batteries included!
Copyright   : (c) TotallyNotChase, 2021
License     : MIT
Maintainer  : totallynotchase42@gmail.com
Stability   : Stable
Portability : Portable

This module exports the primary validator building functions. It also exports all of "Valida.Combinators".

Refer to the hackage documentation for function reference and examples.
You can also find examples in the README, and also in the github repo, within the examples directory.
-}

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

import Data.Bifunctor (Bifunctor (first))

import Valida.Combinators
import Valida.Validation      (Validation (..))
import Valida.ValidationRule  (ValidationRule (..), vrule)
import Valida.ValidationUtils
import Valida.Validator       (Selector, Validator (..))

{- | Build a validator from a 'ValidationRule' and a 'Selector'.

The 'Validator` first runs given __selector__ on its input to obtain the validation target. Then, it runs the
'ValidationRule' on the target.

If validation is successful, the validation target is put into the 'Validation' result.

==== __Examples__

This is the primary function for building validators for your record types.
To validate a pair, the most basic record type, such that the first element is a non empty string, and the second
element is a number greater than 9, you can use:

>>> let pairValidator = (,) <$> verify (notEmpty "EmptyString") fst <*> verify (failureIf (<10) "LessThan10") snd

You can then run the validator on your input, using 'runValidator':
>>> runValidator pairValidator ("foo", 12)
Success ("foo",12)
>>> runValidator pairValidator ("", 12)
Failure ("EmptyString" :| [])
>>> runValidator pairValidator ("foo", 9)
Failure ("LessThan10" :| [])
>>> runValidator pairValidator ("", 9)
Failure ("EmptyString" :| ["LessThan10"])
-}
verify :: ValidationRule e b -> Selector a b -> Validator e a b
verify (ValidationRule rule) selector = Validator $ \x -> selector x <$ rule (selector x)

-- | A synonym for 'verify' with its arguments flipped.
infix 5 -?>

(-?>) :: Selector a b -> ValidationRule e b -> Validator e a b
(-?>) = flip verify

---------------------------------------------------------------------
-- Reassigning corresponding error to 'ValidationRule'.
---------------------------------------------------------------------

{- | Relabel a 'ValidationRule' with a different error.

Many combinators, like 'failureIf'' and 'failureUnless'', simply return the given error value
within /NonEmpty/ upon failure. You can use 'label' to override this return value.

==== __Examples__

>>> let rule = label "NotEven" (failureUnless' even)
>>> runValidator (validate rule) 1
Failure "NotEven"

>>> let rule = label "DefinitelyNotEven" (failureUnless even "NotEven")
>>> runValidator (validate rule) 1
Failure "DefinitelyNotEven"
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

{- | Relabel a 'Validator' with a different error.

==== __Examples__

>>> let validator = labelV "NotEven" (validate (failureUnless' even))
>>> runValidator validator 1
Failure "NotEven"

>>> let validator = labelV "DefinitelyNotEven" (validate (failureUnless even "NotEven"))
>>> runValidator validator 1
Failure "DefinitelyNotEven"
-}
labelV :: e -> Validator x inp a -> Validator e inp a
labelV err (Validator v) = Validator $ first (const err) . v

-- | A synonym for 'labelV' with its arguments flipped.
infix 6 <??>

(<??>) :: Validator x inp a -> e -> Validator e inp a
(<??>) = flip labelV

{- | Build a basic validator from a 'ValidationRule'.

The 'Validator' runs the rule on its input. If validation is successful, the input is put into the 'Validation'
result.

@validate rule = 'verify' rule 'id'@
-}
validate :: ValidationRule e a -> Validator e a a
validate = (-?>) id
