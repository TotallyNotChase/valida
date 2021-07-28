module Valida
    ( Selector
    , Validation (..)
    , ValidationRule
    , Validator
    , buildValidator
    , failureIf
    , failureIf'
    , failureUnless
    , failureUnless'
    , label
    , toEither
    , vrule
    , (-?-)
    , (<?>)
    ) where

import Valida.Combinators    (failureIf, failureIf', failureUnless, failureUnless', label, (<?>))
import Valida.Validation     (Validation (..))
import Valida.ValidationRule (ValidationRule (..), vrule)
import Valida.Validator      (Selector, Validator (..))

-- | Build a validator from a 'ValidationRule' and a 'Selector'.
buildValidator :: ValidationRule e b -> Selector a b -> Validator e a b
buildValidator (ValidationRule rule) selector = Validator $ rule . selector

-- | A synonym for 'buildValidator' with its arguments flipped.
infix 5 -?-

(-?-) :: Selector a b -> ValidationRule e b -> Validator e a b
(-?-) = flip buildValidator

{- | Convert a 'Validation' to an 'Either'.

Given, __Validation a b__-

  * __Failure a__ is converted to __Left a__.
  * __Success b__ is converted to __Right b__.
-}
toEither :: Validation a b -> Either a b
toEither (Failure e) = Left e
toEither (Success a) = Right a

{-
const LoginData = object({ expires: number });

const UserData = object({
  username: string,
  password: string,
  login: option(string),
  sessions: map(string, LoginData),
  type: union(literal('a'), literal('b'), literal('c')),
});
-}
