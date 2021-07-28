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

{- | An applicative validator. Validates a predicate on an input when run.

The Functor instance maps a function over the 'Validation' result by re-using 'fmap' on it.

The Applicative instance can be understood as-
@
(Validator ff) <*> (Validator v) = Validator
    $ (\inp -> ff inp <*> v inp)
@
i.e Run `ff` and `v` on the input, and compose the results (of type 'Validation') with '<*>'.

The 'pure' implementation creates a 'Validator' that, when run, takes __any__ input, /ignores it/, and returns
the given value wrapped in 'Success'.
-}
newtype Validator e inp a = Validator { validate :: inp -> Validation e a }

instance Semigroup e => Functor (Validator e inp) where
    -- | Run the validator function, and `fmap` given function over the result.
    fmap f (Validator v) = Validator $ (f <$>) . v

instance Semigroup e => Applicative (Validator e inp) where
    -- | Validator taking any input and returning given value in `Success`.
    pure = Validator . const . Success
    {- | With minimal pointfree, this is equivalent to-
    ```
    (Validator ff) <*> (Validator v) = Validator
        $ (\inp -> ff inp <*> v inp)
    ```
    i.e Run `ff` and `v` on the input, and compose the results (of type `Validation`) with `<*>`.
    With `ApplicativeDo`, this can be understood as-
    ```
    (Validator ff) <*> (Validator v) = Validator (\inp -> do
        f <- ff inp
        a <- v inp
        pure (f a))
    ```
    -}
    (Validator ff) <*> (Validator v) = Validator $ (<*>) <$> ff <*> v

-- | Build a validator from a 'ValidationRule' and a 'Selector'.
buildValidator :: ValidationRule e b -> Selector a b -> Validator e a b
buildValidator (ValidationRule errF predc) selector = Validator
    $ \y -> let target = selector y
        in if predc target
            then Success target
            else Failure $ errF target

-- | A synonym for 'buildValidator' with its arguments flipped.
infix 5 -?-

(-?-) :: Selector a b -> ValidationRule e b -> Validator e a b
(-?-) = flip buildValidator
