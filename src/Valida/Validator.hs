{-# LANGUAGE DeriveGeneric #-}

module Valida.Validator
    ( Selector
    , Validator (..)
    ) where

import Control.Applicative (Applicative (liftA2))
import Data.Typeable       (Typeable)
import GHC.Generics        (Generic)

import Valida.Validation (Validation (..))

-- | Convenience alias for functions that "select" a record field.
type Selector a b = a -> b

-- | An applicative validator. Validates a predicate on an input when run and returns the 'Validation' result.
newtype Validator e inp a = Validator { validate :: inp -> Validation e a } deriving (Typeable, Generic)

{- |
* 'fmap' maps given function over the 'Validation' result by re-using 'fmap' on it.
-}
instance Functor (Validator e inp) where
    fmap f (Validator v) = Validator $ fmap f . v

{- |
* 'pure' creates a 'Validator' that always yields given value wrapped in 'Success', ignoring its input.
* '(<*>)' runs 2 validators to obtain the 2 'Validation' results and combines them with '(<*>)'.
This can be understood as-

    @
    (Validator ff) \<*\> (Validator v) = Validator (\\inp -> ff inp \<*\> v inp)
    @

    i.e Run __ff__ and __v__ on the input, and compose the 'Validation' results with '(<*>)'.
-}
instance Semigroup e => Applicative (Validator e inp) where
    pure = Validator . const . Success
    Validator ff <*> Validator v = Validator $ liftA2 (<*>) ff v

{- |
* '(<>)' applies input over both validator functions, and combines the 'Validation' results using '(<>)'.
-}
instance Semigroup e => Semigroup (Validator e inp a) where
    Validator f <> Validator g = Validator $ f <> g
