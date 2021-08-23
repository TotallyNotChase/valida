{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Safe              #-}

module Valida.Validator
    ( Validator (..)
    ) where

import Data.List.NonEmpty (NonEmpty)
import Data.Profunctor    (Profunctor (lmap, rmap))
import Data.Typeable      (Typeable)

import GHC.Generics (Generic)

import Valida.Validation (Validation (..))

{- | An applicative validator. Validates a predicate on an input when run and returns the 'Validation' result.

The type can be understood as-

    Validator e inp a
              ^   ^ ^------ The output type on successful validation
              |   |
      Error type  |-- The input on which validation is run

Validators are run using the __runValidator__ function.
The result is of type __Validation e a__, corresponding to the type params of the same name on 'Validator'.

__Note__: All the primitive combinators (and derivative combinators) use the same type for @inp@ and @a@.
In those cases - upon successful validation, the input itself, wrapped in 'Success', is returned.
-}
newtype Validator e inp a = Validator { runValidator :: inp -> Validation e a }
  deriving (Typeable, Generic)

{- |
[@fmap@] 'fmap' maps given function over the 'Validation' result by re-using 'fmap' on it.

__Examples__

>>> runValidator (fmap (+1) (failureIf (==2) "IsTwo")) 3
Success 4
>>> runValidator (fmap (+1) (failureIf (==2) "IsTwo")) 2
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
>>> runValidator (const <$> failureIf (==2) "IsTwo" <*> failureIf even "IsEven") 5
Success 5
>>> runValidator (const <$> failureIf (==2) "IsTwo" <*> failureIf even "IsEven") 4
Failure ("IsEven" :| [])
>>> runValidator (const <$> failureIf (==2) "IsTwo" <*> failureIf even "IsEven") 2
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
[@(<>)@] '(<>)' builds a validator that /succeeds/ only if both of the given validators succeed.
__Left-most__ failure is returned, other validator /is not used/ if one fails. If all validators succeed,
__right-most__ success is returned.

__Examples__

>>> let v1 = fixV (failureIf (==2) "IsTwo")
>>> let v2 = fixV (failureIf even "IsEven")
>>> runValidator (v1 <> v2) 5
Success 5
>>> runValidator (v1 <> v2) 4
Failure ("IsEven" :| [])
>>> runValidator (v1 <> v2) 2
Failure ("IsTwo" :| [])
-}
instance Semigroup (Validator e inp a) where
    Validator v1 <> Validator v2 = Validator $ \x -> case (v1 x, v2 x) of
        (f@(Failure _), _) -> f
        (_, b)             -> b
{- |

[@mempty@] 'mempty' is a validator that always succeeds and uses /unit/ as output type.

__Examples__

>>> runValidator (mempty :: Validator String Int ()) 42
Success ()
-}
instance Monoid (Validator e inp ()) where
    mempty = Validator $ const $ Success ()

{- |

[@lmap@] 'lmap' runs given function on 'Validator' input before applying it to the validator function.
This is similar to the 'Data.Contravariant.Predicate' type.

[@rmap@] 'rmap' is the same as 'fmap'.

__Examples__

>>> runValidator (lmap fst (fixV $ failureIf (==2) "IsTwo")) (3, 2)
Success 3
>>> runValidator (lmap snd (fixV $ failureIf (==2) "IsTwo")) (3, 2)
Failure ("IsTwo" :| [])
>>> runValidator (rmap (+1) (fixV $ failureIf (==2) "IsTwo")) 3
Failure ("IsTwo" :| [])
-}
instance Profunctor (Validator e) where
    lmap selector (Validator v) = Validator $ v . selector
    rmap = fmap
