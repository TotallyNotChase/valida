module Valida.Validation
    ( Validation (..)
    ) where

-- | Like 'Either', but accumulates failures upon applicative composition.
data Validation e a
  -- | Represents a validation failure with an error.
  = Failure e
  -- | Represents a successful validation with the validated value.
  | Success a
  deriving (Eq, Show)

{- |
* 'fmap' maps given function over a 'Success' value, does nothing on 'Failure' value.
-}
instance Functor (Validation e) where
    fmap _ (Failure e) = Failure e
    fmap f (Success a) = Success $ f a

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
