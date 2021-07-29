module Valida
    ( Selector
      -- | Re-exports of "Valida.Combinators".
    , module Valida.Combinators
    , Validation (..)
    , ValidationRule
    , Validator (validate)
    , fromEither
    , select
    , toEither
    , verify
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

{- | Build a basic validator from a 'ValidationRule'.

The 'Validator' runs the rule on its input. If validation is successful, the input is put into the 'Validation'
result.

prop> verify = flip select id
-}
verify :: ValidationRule e a -> Validator e a a
verify = (-?-) id
