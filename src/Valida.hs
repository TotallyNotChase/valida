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

import Valida.Validation     (Validation (..), toEither)
import Valida.ValidationRule (ValidationRule, failureIf, failureIf', failureUnless, failureUnless', label, vrule, (<?>))
import Valida.Validator      (Selector, Validator, buildValidator, (-?-))

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
