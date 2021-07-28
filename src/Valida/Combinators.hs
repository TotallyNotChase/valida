module Valida.Combinators
    ( failureIf
    , failureIf'
    , failureUnless
    , failureUnless'
    , label
    , (<?>)
    ) where

import Data.List.NonEmpty (NonEmpty)

import Valida.Utils          (singleton)
import Valida.Validation     (Validation (..))
import Valida.ValidationRule (ValidationRule (..), vrule)

-- | Build a predicate that /fails/ with given error __if the given predicate succeeds__.
failureIf :: (a -> Bool) -> e -> ValidationRule (NonEmpty e) a
failureIf predc = predToRule (not . predc) . singleton

-- | Like 'failureIf' but uses 'Data.Unit' as the 'ValidationRule' error type.
failureIf' :: (a -> Bool) -> ValidationRule () a
failureIf' = flip predToRule () . (not .)

-- | Build a predicate that /fails/ with given error __unless the given predicate succeeds__.
failureUnless :: (a -> Bool) -> e -> ValidationRule (NonEmpty e) a
failureUnless predc = predToRule predc . singleton

-- | Like 'failureUnless' but uses /Unit/ as the 'ValidationRule' error type.
failureUnless' :: (a -> Bool) -> ValidationRule () a
failureUnless' = flip predToRule ()

-- | Relabel a 'ValidationRule' with a new error generator.
label :: (a -> e) -> ValidationRule x a -> ValidationRule e a
label errF (ValidationRule rule) = vrule $ \x -> case rule x of
    Success a -> Success a
    Failure _ -> Failure (errF x)

-- | A synonym for 'label' with its arguments flipped.
infix 6 <?>

(<?>) :: ValidationRule x a -> (a -> e) -> ValidationRule e a
(<?>) = flip label

-- | Utility to convert a regular predicate function to a 'ValidationRule'. __INTERNAL__
predToRule :: (a -> Bool) -> e -> ValidationRule e a
predToRule predc err = vrule $ \x -> if predc x
    then Success ()
    else Failure err
