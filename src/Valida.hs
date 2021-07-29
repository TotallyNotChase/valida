module Valida
    ( Selector
      -- | Re-exports of "Valida.Combinators".
    , module Valida.Combinators
    , Validation (..)
    , ValidationRule
    , Validator
    , select
    , fromEither
    , toEither
    , vrule
    , (-?-)
    ) where

import Valida.Combinators
import Valida.Validation      (Validation (..))
import Valida.ValidationRule  (ValidationRule (..), vrule)
import Valida.ValidationUtils (fromEither, toEither)
import Valida.Validator       (Selector, Validator (..))

{- | Build a validator from a 'ValidationRule' and a 'Selector'.

The 'Validator` first runs given __selector__ on its input to obtain the validation target. Then, it runs the
'ValidationRule' on the target.

If validation is successful, the validation target is put into the 'Validation' result.
-}
select :: ValidationRule e b -> Selector a b -> Validator e a b
select (ValidationRule rule) selector = Validator $ (<$) <$> selector <*> (rule . selector)

-- | A synonym for 'select' with its arguments flipped.
infix 5 -?-

(-?-) :: Selector a b -> ValidationRule e b -> Validator e a b
(-?-) = flip select

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
