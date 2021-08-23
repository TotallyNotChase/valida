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
      --
    , selectV
    , verify
      --
    , fixV
    , (-?>)
    , (-|?>)
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

{- | An alias to 'lmap' specialized to 'Validator'.

'selectV' allows a validator taking input 'b' to work with input 'a', provided a function of type: @a -> b@.

The new 'Validator` first runs the __selector__ on its input to obtain the validation target. Then, it runs the
predicate on the target.

If validation is successful, the the *original* output is put into the 'Validation' result.

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
selectV :: Validator e b x -> (a -> b) -> Validator e a x
selectV = flip lmap

{- | 'fixV' given validator, and 'selectV' (i.e 'lmap') given selector over it.

This is the same as 'selectV' except that it 'fixV's the given validator first. On already fixed validators (such as
those returned by all primitive and derivative combinators), 'verify' is the same as 'selectV'.
-}
verify :: Validator e b x -> (a -> b) -> Validator e a b
verify vald selector = selector -|?> fixV vald

-- | A synonym for 'verify' with its arguments flipped.
infix 5 -?>

(-?>) :: (a -> b) -> Validator e b x -> Validator e a b
(-?>) = flip verify

-- | A synonym for 'selectV' with its arguments flipped.
infix 5 -|?>

(-|?>) :: (a -> b) -> Validator e b x -> Validator e a x
(-|?>) = flip selectV

{- | Fix a validator's output to be the same as its input.

@fixV . fixV = 'id' . fixV@
@'fmap' ('const' x) .  fixV = 'fmap' ('const' x)@

__Note__: The primitive and derivative combinators already fix the validator output to be the
same as its input.

==== __Examples__

This is useful for regaining the input value in the output position after multiple 'fmap's.

Assume we have a validator that fails when input number is even-
>>> let evenValidator = failureIf even "Even"

This validator, when run, will yield its input value, wrapped in 'Success', if input is not even. 'fixV' would be redundant on this.

However, if the output was 'fmap'ed to be something else-
>>> let evenValidator' = fmap (:[]) evenValidator

Now the output type is `[Int]`. The value of the output is no longer the same as the input. If we needed to get the
original input back into the output, 'fixV' would be the right choice.

>>> let evenValidator'' = fixV evenValidator'

'evenValidator''' is now the exact same as 'evenValidator', which was fixed from the start.

>>> (("foo" <$ failureIf even "Even") <> ("bar" <$ failureIf (<0) "Negative")) `runValidator` 5
Success "bar"
>>> fixV (("foo" <$ failureIf even "Even") <> ("bar" <$ failureIf (<0) "Negative")) `runValidator` 5
Success 5
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
