{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE Safe          #-}

module Valida.Validator
    ( Selector
    , Validator (..)
    ) where

import Control.Applicative (Applicative (liftA2))
import Data.List.NonEmpty  (NonEmpty)
import Data.Typeable       (Typeable)
import GHC.Generics        (Generic)

import Valida.Validation (Validation (..))

-- | Convenience alias for functions that "select" a record field.
type Selector a b = a -> b

-- | An applicative validator. Validates a predicate on an input when run and returns the 'Validation' result.
newtype Validator e inp a = Validator { runValidator :: inp -> Validation e a } deriving (Typeable, Generic)

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
    {-# SPECIALIZE instance Applicative (Validator (NonEmpty err) inp) #-}
    {-# SPECIALIZE instance Applicative (Validator () inp) #-}
    {-# SPECIALIZE instance Applicative (Validator [err] inp) #-}
    pure = Validator . const . Success
    {-# INLINEABLE pure #-}
    Validator ff <*> Validator v = Validator $ liftA2 (<*>) ff v
    {-# INLINEABLE (<*>) #-}

{- |
* '(<>)' applies input over both validator functions, and combines the 'Validation' results using '(<>)'.
-}
instance Semigroup e => Semigroup (Validator e inp a) where
    {-# SPECIALIZE instance Semigroup (Validator (NonEmpty err) inp a) #-}
    {-# SPECIALIZE instance Semigroup (Validator () inp a) #-}
    {-# SPECIALIZE instance Semigroup (Validator [err] inp a) #-}
    Validator f <> Validator g = Validator $ f <> g
    {-# INLINEABLE (<>) #-}
