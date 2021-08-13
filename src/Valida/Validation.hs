{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}

module Valida.Validation
    ( Validation (..)
    , validation
    ) where

import Data.Bifoldable    (Bifoldable (bifoldMap))
import Data.Bifunctor     (Bifunctor (bimap))
import Data.Bitraversable (Bitraversable)
import Data.Data          (Data)
import Data.Typeable      (Typeable)
import GHC.Generics       (Generic)

-- | Like 'Either', but accumulates failures upon applicative composition.
data Validation e a
  -- | Represents a validation failure with an error.
  = Failure e
  -- | Represents a successful validation with the validated value.
  | Success a
  deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

{- |
* 'fmap' maps given function over a 'Success' value, does nothing on 'Failure' value.
-}
instance Functor (Validation e) where
    fmap _ (Failure e) = Failure e
    fmap f (Success a) = Success $ f a

instance Bifunctor Validation where
    bimap f g = validation (Failure . f) (Success . g)

{- |
* 'pure' is a 'Success' value.
* '(<*>)' behaves similar to 'Either', but accumulates failures instead of stopping.
-}
instance Semigroup e => Applicative (Validation e) where
    pure = Success
    Success f <*> Success b = Success $ f b
    Success _ <*> Failure e = Failure e
    Failure e <*> Success _ = Failure e
    Failure x <*> Failure y = Failure $ x <> y

{- |
* '(<>)' behaves similar to the 'Either' semigroup. i.e Returns the first 'Success'. But also accumulates 'Failure's.
-}
instance Semigroup e => Semigroup (Validation e a) where
    s@(Success _) <> _             = s
    _             <> s@(Success _) = s
    Failure x     <> Failure y     = Failure $ x <> y

instance Foldable (Validation e) where
    foldMap = validation (const mempty)

instance Traversable (Validation e) where
    traverse f = validation (pure . Failure) (fmap Success . f)

instance Bifoldable Validation where
    bifoldMap = validation

instance Bitraversable Validation

{- | Case analysis for 'Validation', i.e catamorphism.

In case of 'Failure e', apply the first function to e; in case of 'Success a', apply the second function to a.
-}
validation :: (e -> c) -> (a -> c) -> Validation e a -> c
validation ef _ (Failure e) = ef e
validation _ af (Success a) = af a
