module Valida.Validator
    ( Selector
    , Validator (..)
    , buildValidator
    , (-?-)
    ) where

import Valida.Validation     (Validation (..))
import Valida.ValidationRule (ValidationRule (..))

-- | Convenience alias for functions that "select" a record field.
type Selector a b = a -> b

-- | An applicative validator. Validates a predicate on an input when run.
newtype Validator e inp a = Validator { validate :: inp -> Validation e a }

instance Semigroup e => Functor (Validator e inp) where
    -- | Run the validator function, and `fmap` given function over the result.
    fmap f (Validator v) = Validator $ (f <$>) . v

instance Semigroup e => Applicative (Validator e inp) where
    pure a = Validator (\_ -> Success a)
    (Validator ff) <*> (Validator v) = Validator $ (<*>) <$> ff <*> v

buildValidator :: ValidationRule e b -> Selector a b -> Validator e a b
buildValidator (ValidationRule errF predc) selector = Validator
    $ \y -> let target = selector y
        in if predc target
            then Success target
            else Failure $ errF target

infix 5 -?-

(-?-) :: Selector a b -> ValidationRule e b -> Validator e a b
(-?-) = flip buildValidator
