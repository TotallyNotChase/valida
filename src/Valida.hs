{-# LANGUAGE Safe #-}

{- |
Module      : Valida
Description : Core Validator builders, utilities, and combinators
Copyright   : (c) TotallyNotChase, 2021
License     : MIT
Maintainer  : totallynotchase42@gmail.com
Stability   : Stable
Portability : Portable

This module exports the primary validator building functions. It also exports all of "Valida.Combinators".

For a full tutorial, check out the README at <https://github.com/TotallyNotChase/valida#readme>.
Refer to the hackage documentation for function reference and examples.

You can also find more examples within the examples directory in linked github repo.
-}

module Valida
    ( -- * Primary data types
      Validation (..)
    , Validator (runValidator)
      -- * Building and modifying 'Validator's
    , fixV
    , verify
    , (-?>)
      -- * Reassigning errors
    , label
    , (<?>)
      -- | Re-exports of "Valida.Combinators"
    , module Valida.Combinators
    , module Valida.ValidationUtils
    ) where

import Data.Bifunctor  (Bifunctor (first))
import Data.Profunctor (Profunctor (lmap))

import Valida.Combinators
import Valida.Validation      (Validation (..))
import Valida.ValidationUtils
import Valida.Validator       (Validator (..))

{- | 'fixV' given validator, and 'lmap' given selector over it.

The new 'Validator` first runs the __selector__ on its input to obtain the validation target. Then, it runs the
predicate on the target.

If validation is successful, the the *original* input (not validation target) is put into the 'Validation' result.

==== __Examples__

This is the primary function for building validators for product types.
To validate a pair, the most basic product type, such that the first element is a non empty string, and the second
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
verify :: Validator e b x -> (a -> b) -> Validator e a b
verify vald selector = lmap selector $ fixV vald

-- | A synonym for 'verify' with its arguments flipped.
infix 5 -?>

(-?>) :: (a -> b) -> Validator e b x -> Validator e a b
(-?>) = flip verify

{- | Fix a validator's output to be the same as its input.

@fixV . fixV = 'id' . fixV@
@'fmap' ('const' x) .  fixV = 'fmap' ('const' x)@

==== __Examples__

The combinators from "Valida.Combinators" use /Unit/ as 'Validator' output type.
This is important for the 'Validator' semigroup operations to /make sense/.
Once the 'Validator' has been built as you desire, and encodes all "rules" for validation - use fixV to fix its output to its input.

This allows for the applicative validation flow. The 'Validator' needs to yield its input, as output, so it can be composed using '(<*>)'.

>>> runValidator (fixV $ failureIf even "Even" <> failureIf (<0) "Negative") 5
Success 5
>>> runValidator (fixV $ failureIf even "Even" <> failureIf (<0) "Negative") 2
Failure ("Even" :| [])
>>> runValidator (fixV $ failureIf even "Even" <> failureIf (<0) "Negative") (-3)
Failure ("Negative" :| [])
>>> runValidator (fixV $ failureIf even "Even" <> failureIf (<0) "Negative") (-2)
Failure ("Even" :| [])

Without 'fixV', it'd instead yield 'Success ()' on success.
>>> runValidator (failureIf even "Even" <> failureIf (<0) "Negative") 5

-}
fixV :: Validator e a x -> Validator e a a
fixV (Validator v) = Validator $ \x -> x <$ v x

---------------------------------------------------------------------
-- Reassigning corresponding error to 'Validator'.
---------------------------------------------------------------------

{- | Relabel a 'Validator' with a different error.

==== __Examples__

>>> let validator = label "NotEven" (failureUnless' even)
>>> runValidator validator 1
Failure "NotEven"

>>> let validator = label "DefinitelyNotEven" (failureUnless even "NotEven")
>>> runValidator validator 1
Failure "DefinitelyNotEven"
-}
label :: e -> Validator x inp a -> Validator e inp a
label err (Validator v) = Validator $ first (const err) . v

-- | A synonym for 'label' with its arguments flipped.
infix 6 <?>

(<?>) :: Validator x inp a -> e -> Validator e inp a
(<?>) = flip label
