{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE Safe               #-}

module Valida.Validation
    ( Validation (..)
    , validation
    , validationConst
    ) where

import Data.Bifoldable    (Bifoldable (bifoldMap))
import Data.Bifunctor     (Bifunctor (bimap))
import Data.Bitraversable (Bitraversable)
import Data.Data          (Data)
import Data.List.NonEmpty (NonEmpty)
import Data.Typeable      (Typeable)

import GHC.Generics (Generic)

-- | Like 'Either', but accumulates failures upon applicative composition.
data Validation e a
  -- | Represents a validation failure with an error.
  = Failure e
  -- | Represents a successful validation with the validated value.
  | Success a
  deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

{- |

[@fmap@] 'fmap' maps given function over a 'Success' value, does nothing on 'Failure' value.

__Examples__

>>> fmap (+1) (Success 2)
Success 3
>>> fmap (+1) (Failure "error")
Failure "error"
-}
instance Functor (Validation e) where
    fmap _ (Failure e) = Failure e
    fmap f (Success a) = Success $ f a

instance Bifunctor Validation where
    bimap f g = validation (Failure . f) (Success . g)

{- |
[@pure@] 'pure' is a 'Success' value.
[@(\<*\>)@] '(<*>)' behaves similar to 'Either', but accumulates failures instead of stopping.

__Examples__

>>> pure 2 :: Validation String Int
Success 2
>>> Success (+1) <*> Success 4
Success 5
>>> Success (+1) <*> Failure "error"
Failure "error"
>>> Failure ["err1"] <*> Failure ["err2"]
Failure ["err1","err2"]
-}
instance Semigroup e => Applicative (Validation e) where
    {-# SPECIALIZE instance Applicative (Validation (NonEmpty err)) #-}
    {-# SPECIALIZE instance Applicative (Validation ()) #-}
    {-# SPECIALIZE instance Applicative (Validation [err]) #-}
    pure = Success
    {-# INLINEABLE pure #-}
    Success f <*> Success b = Success $ f b
    Success _ <*> Failure e = Failure e
    Failure e <*> Success _ = Failure e
    Failure x <*> Failure y = Failure $ x <> y
    {-# INLINEABLE (<*>) #-}

{- |
[@(<>)@] This behaves similar to the 'Either' semigroup. i.e Returns the first 'Success'. But also accumulates 'Failure's.

__Examples__

>>> Success 1 <> Success 2
Success 1
>>> Failure "error" <> Success 1
Success 1
>>> Success 2 <> Failure "error"
Success 2
>>> Failure ["err1"] <> Failure ["err2"]
Failure ["err1","err2"]
-}
instance Semigroup e => Semigroup (Validation e a) where
    {-# SPECIALIZE instance Semigroup (Validation (NonEmpty err) a) #-}
    {-# SPECIALIZE instance Semigroup (Validation () a) #-}
    {-# SPECIALIZE instance Semigroup (Validation [err] a) #-}
    s@(Success _) <> _             = s
    _             <> s@(Success _) = s
    Failure x     <> Failure y     = Failure $ x <> y
    {-# INLINEABLE (<>) #-}

{- |

[@foldMap@] 'foldMap' maps given function over a 'Success' value, returns 'mempty' for a 'Failure' value.

__Examples__

>>> foldMap (:[]) (Success 2)
[2]
>>> foldMap (:[]) (Failure "error")
[]
-}
instance Foldable (Validation e) where
    foldMap = validation (const mempty)

{- |

[@traverse@] In case of 'Success', 'traverse' applies given function to the inner value, and maps 'Success' over the result.
In case of 'Failure', 'traverse' returns 'Failure', wrapped in minimal context of the corresponding type ('pure').

__Examples__

>>> traverse Just (Success 2)
Just (Success 2)
>>> traverse Just (Failure "error")
Just (Failure "error")
-}
instance Traversable (Validation e) where
    traverse f = validation (pure . Failure) (fmap Success . f)

{- |

[@bifoldMap@] 'bifoldMap' is the same as 'validation'.

__Examples__

'bifoldMap' (and its more generalized version, 'validation') can eliminate the need to pattern match on 'Validation'.

>>> import Data.Bifoldable
>>> bifoldMap reverse (:[]) (Success 'c' :: Validation String Char)
"c"
>>> bifoldMap reverse (:[]) (Failure "error" :: Validation String Char)
"rorre"
-}
instance Bifoldable Validation where
    bifoldMap = validation

instance Bitraversable Validation

{- | Case analysis for 'Validation', i.e catamorphism.

In case of 'Failure e', apply the first function to e; in case of 'Success a', apply the second function to a.

This is a more generalized version of the 'bifoldMap' implementation.

==== __Examples__

>>> validation (const Nothing) Just (Success 'c' :: Validation String Char)
Just 'c'
>>> validation (const Nothing) Just (Failure "error" :: Validation String Char)
Nothing
-}
validation :: (e -> c) -> (a -> c) -> Validation e a -> c
validation ef _ (Failure e) = ef e
validation _ af (Success a) = af a

{- | Case analysis for 'Validation', with replacer.

This is similar to 'validation', but takes in replacers instead of functions.

In case of 'Failure', return the first argument; otherwise, return the second argument.

@validationConst e a = 'validation' ('const' e) ('const' a)@

==== __Examples__

>>> validation (const Nothing) Just (Success 'c' :: Validation String Char)
Just 'c'
>>> validation (const Nothing) Just (Failure "error" :: Validation String Char)
Nothing
-}
validationConst :: p -> p -> Validation e a -> p
validationConst e _ (Failure _) = e
validationConst _ a _           = a
