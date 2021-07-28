module Valida.Validation
    ( Validation (..)
    , toEither
    ) where

-- | Like 'Either', but accumulates failures upon applicative composition.
data Validation e a
  -- | Represents a validation failure with an error.
  = Failure e
  -- | Represents a successful validation with the validated value.
  | Success a

instance Semigroup e => Functor (Validation e) where
    fmap _ (Failure e) = Failure e
    fmap f (Success a) = Success $ f a

instance Semigroup e => Applicative (Validation e) where
    pure = Success
    (Success f) <*> (Success b)   = Success $ f b
    (Success _) <*> (Failure e)   = Failure e
    (Failure e) <*> (Success _)   = Failure e
    (Failure e1) <*> (Failure e2) = Failure $ e1 <> e2

{- | Convert a 'Validation' to an 'Either'.

Given, __Validation a b__-

  * __Failure a__ is converted to __Left a__.
  * __Success b__ is converted to __Right b__.
-}
toEither :: Validation a b -> Either a b
toEither (Failure e) = Left e
toEither (Success a) = Right a
