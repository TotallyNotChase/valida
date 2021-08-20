{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE Safe          #-}

module Valida.Validator
    ( Selector
    , Validator (..)
    ) where

import Data.List.NonEmpty (NonEmpty)
import Data.Typeable      (Typeable)

import GHC.Generics (Generic)

import Valida.Validation (Validation (..))

-- | Convenience alias for functions that "select" a record field.
type Selector a b = a -> b

-- | An applicative validator. Validates a predicate on an input when run and returns the 'Validation' result.
newtype Validator e inp a = Validator { runValidator :: inp -> Validation e a }
  deriving (Typeable, Generic)

{- |

[@fmap@] 'fmap' maps given function over the 'Validation' result by re-using 'fmap' on it.

__Examples__

>>> runValidator (fmap (+1) (validate $ failureIf (==2) "IsTwo")) 3
Success 4
>>> runValidator (fmap (+1) (validate $ failureIf (==2) "IsTwo")) 2
Failure ("IsTwo" :| [])
-}
instance Functor (Validator e inp) where
    fmap f (Validator v) = Validator $ fmap f . v

{- |

[@pure@] 'pure' creates a 'Validator' that always yields given value wrapped in 'Success', ignoring its input.

[@(\<*\>)@] '(<*>)' runs 2 validators to obtain the 2 'Validation' results and combines them with '(<*>)'.
This can be understood as-

    @
    (Validator ff) \<*\> (Validator v) = Validator (\\inp -> ff inp \<*\> v inp)
    @

    i.e Run __ff__ and __v__ on the input, and compose the 'Validation' results with '(<*>)'.

__Examples__

>>> runValidator (pure 5) 42
Success 5
>>> let v1 = validate (failureIf (==2) "IsTwo")
>>> let v2 = validate (failureIf even "IsEven")
>>> runValidator (const <$> v1 <*> v2) 5
Success 5
>>> runValidator (const <$> v1 <*> v2) 4
Failure ("IsEven" :| [])
>>> runValidator (const <$> v1 <*> v2) 2
Failure ("IsTwo" :| ["IsEven"])
-}
instance Semigroup e => Applicative (Validator e inp) where
    {-# SPECIALIZE instance Applicative (Validator (NonEmpty err) inp) #-}
    {-# SPECIALIZE instance Applicative (Validator () inp) #-}
    {-# SPECIALIZE instance Applicative (Validator [err] inp) #-}
    pure = Validator . const . Success
    {-# INLINEABLE pure #-}
    Validator ff <*> Validator v = Validator $ \x -> ff x <*> v x
    {-# INLINEABLE (<*>) #-}

{- |

[@(<>)@] '(<>)' applies input over both validator functions, and combines the 'Validation' results using '(<>)'.

__Examples__

This essentially reuses the '(<>)' impl of 'Validation'.
i.e Returns the first 'Success'. But also accumulates 'Failure's.

>>> let v1 = validate (failureIf (==2) "IsTwo")
>>> let v2 = validate (failureIf even "IsEven")
>>> runValidator (v1 <> v2) 5
Success 5
>>> runValidator (v1 <> v2) 4
Success 4
>>> runValidator (v1 <> v2) 2
Failure ("IsTwo" :| ["IsEven"])
-}
instance Semigroup e => Semigroup (Validator e inp a) where
    {-# SPECIALIZE instance Semigroup (Validator (NonEmpty err) inp a) #-}
    {-# SPECIALIZE instance Semigroup (Validator () inp a) #-}
    {-# SPECIALIZE instance Semigroup (Validator [err] inp a) #-}
    Validator f <> Validator g = Validator $ f <> g
    {-# INLINEABLE (<>) #-}
