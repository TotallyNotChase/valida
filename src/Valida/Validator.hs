module Valida.Validator
    ( Selector
    , Validator (..)
    ) where

import Control.Applicative (Applicative (liftA2))

import Valida.Validation (Validation (..))

-- | Convenience alias for functions that "select" a record field.
type Selector a b = a -> b

{- | An applicative validator. Validates a predicate on an input when run.

The Functor instance maps a function over the 'Validation' result by re-using 'fmap' on it.

The Applicative instance can be understood as-

@
(Validator ff) <*> (Validator v) = Validator
    $ (\\inp -> ff inp <*> v inp)
@

i.e Run __ff__ and __v__ on the input, and compose the results (of type 'Validation') with '(<*>)'.

The 'pure' implementation creates a 'Validator' that, when run, takes __any__ input, /ignores it/, and returns
the given value wrapped in 'Success'.
-}
newtype Validator e inp a = Validator { validate :: inp -> Validation e a }

instance Functor (Validator e inp) where
    -- | Run the validator function, and __fmap__ given function over the result.
    fmap f (Validator v) = Validator $ fmap f . v

instance Semigroup e => Applicative (Validator e inp) where
    -- | Validator taking any input and returning given value in __Success__.
    pure = Validator . const . Success
    {- | With minimal pointfree, this is equivalent to-

    @
    (Validator ff) <*> (Validator v) = Validator
        $ (\\inp -> ff inp <*> v inp)
    @

    i.e Run __ff__ and __v__ on the input, and compose the results (of type __Validation__) with __<*>__.
    With __ApplicativeDo__, this can be understood as-

    @
    (Validator ff) <*> (Validator v) = Validator (\\inp -> do
        f <- ff inp
        a <- v inp
        pure (f a))
    @
    -}
    Validator ff <*> Validator v = Validator $ liftA2 (<*>) ff v

{- |
* '(<>)' applies the inp over both validator functions, and combines the 'Validation' results using '(<>)'.
-}
instance Semigroup e => Semigroup (Validator e inp a) where
    Validator f <> Validator g = Validator $ f <> g
