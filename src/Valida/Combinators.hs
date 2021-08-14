module Valida.Combinators
    ( -- * Primitive 'NonEmpty' combinators
      failureIf
    , failureUnless
      -- * Primitive /Unit/ combinators
    , failureIf'
    , failureUnless'
      -- * Negating 'ValidationRule'
    , negateRule
    , negateRule'
      -- * Combining 'ValidationRule's
    , andAlso
    , falseRule
    , orElse
    , satisfyAll
    , satisfyAny
    , (</>)
      -- * Common derivates of primitive 'NonEmpty' combinators
    , atleastContains
    , maxLengthOf
    , maxValueOf
    , minLengthOf
    , minValueOf
    , mustBe
    , mustContain
    , notEmpty
    , onlyContains
      -- * Common derivates of primitive /Unit/ combinators
    , atleastContains'
    , maxLengthOf'
    , maxValueOf'
    , minLengthOf'
    , minValueOf'
    , mustBe'
    , mustContain'
    , notEmpty'
    , onlyContains'
    ) where

import Control.Applicative (Applicative (liftA2))
import Data.Foldable       (Foldable (fold))
import Data.List.NonEmpty  (NonEmpty)

import Valida.Utils          (singleton)
import Valida.Validation     (Validation (..))
import Valida.ValidationRule (ValidationRule (..), vrule)

---------------------------------------------------------------------
-- Primitive 'NonEmpty' combinators
---------------------------------------------------------------------

{- | Build a rule that /fails/ with given error __if the given rule succeeds__.

prop> failureIf predc = failureUnless (not . predc)
-}
failureIf :: (a -> Bool) -> e -> ValidationRule (NonEmpty e) a
failureIf predc = predToRule (not . predc) . singleton

{- | Build a rule that /fails/ with given error __unless the given rule succeeds__.

prop> failureUnless predc = failureIf (not . predc)
-}
failureUnless :: (a -> Bool) -> e -> ValidationRule (NonEmpty e) a
failureUnless predc = predToRule predc . singleton

---------------------------------------------------------------------
-- Primitive /Unit/ combinators
---------------------------------------------------------------------

{- | Like 'failureIf' but uses /Unit/ as the 'ValidationRule' error type.

prop> failureIf' predc = failureUnless' (not . predc)
prop> label (const (err :| [])) (failureIf' predc) = failureIf predc err
-}
failureIf' :: (a -> Bool) -> ValidationRule () a
failureIf' = flip predToRule () . (not .)

{- | Like 'failureUnless' but uses /Unit/ as the 'ValidationRule' error type.

prop> failureUnless' predc = failureIf' (not . predc)
prop> label (const (err :| [])) (failureUnless' predc) = failureUnless predc err
-}
failureUnless' :: (a -> Bool) -> ValidationRule () a
failureUnless' = flip predToRule ()

---------------------------------------------------------------------
-- Common derivates of primitive 'NonEmpty' combinators
---------------------------------------------------------------------

{- | Build an equality rule for value.

prop> mustBe x = failureUnless (==x)
-}
mustBe :: Eq a => a -> e -> ValidationRule (NonEmpty e) a
mustBe x = failureUnless (==x)

{- | Build a minimum length (inclusive) rule.

prop> minLengthOf x = failureUnless ((>=n) . length)
-}
minLengthOf :: Foldable t => Int -> e -> ValidationRule (NonEmpty e) (t a)
minLengthOf n = failureUnless ((>=n) . length)

{- | Build a maximum length (inclusive) rule.

prop> maxLengthOf n = failureUnless ((<=n) . length)
-}
maxLengthOf :: Foldable t => Int -> e -> ValidationRule (NonEmpty e) (t a)
maxLengthOf n = failureUnless ((<=n) . length)

{- | Build a maximum length rule.

prop> notEmpty = minLengthOf 1
prop> notEmpty = failureIf null
-}
notEmpty :: Foldable t => e -> ValidationRule (NonEmpty e) (t a)
notEmpty = failureIf null

{- | Build a minimum value (inclusive) rule.

prop> minValueOf x = failureUnless (>=x)
-}
minValueOf :: Ord a => a -> e -> ValidationRule (NonEmpty e) a
minValueOf x = failureUnless (>=x)

{- | Build a maximum value (inclusive) rule.

prop> maxValueOf x = failureUnless (<=x)
-}
maxValueOf :: Ord a => a -> e -> ValidationRule (NonEmpty e) a
maxValueOf x = failureUnless (<=x)

{- | Build an 'all' rule.

prop> onlyContains x = failureUnless (all x)
-}
onlyContains :: Foldable t => (a -> Bool) -> e -> ValidationRule (NonEmpty e) (t a)
onlyContains x = failureUnless (all x)

{- | Build an 'any' rule.

prop> atleastContains x = failureUnless (any x)
-}
atleastContains :: Foldable t => (a -> Bool) -> e -> ValidationRule (NonEmpty e) (t a)
atleastContains x = failureUnless (any x)

{- | Build an 'elem' rule.

prop> mustContain x = atleastContains (==x)

prop> mustContain x = failureUnless (elem x)
-}
mustContain :: (Foldable t, Eq a) => a -> e -> ValidationRule (NonEmpty e) (t a)
mustContain x = failureUnless (elem x)

---------------------------------------------------------------------
-- Common derivates of primitive /Unit/ combinators
---------------------------------------------------------------------

-- | Like 'mustBe' but uses /Unit/ as the 'ValidationRule' error type.
mustBe' :: Eq a => a -> ValidationRule () a
mustBe' x = failureUnless' (==x)

-- | Like 'minLengthOf' but uses /Unit/ as the 'ValidationRule' error type.
minLengthOf' :: Foldable t => Int -> ValidationRule () (t a)
minLengthOf' n = failureUnless' ((>=n) . length)

-- | Like 'maxLengthOf' but uses /Unit/ as the 'ValidationRule' error type.
maxLengthOf' :: Foldable t => Int -> ValidationRule () (t a)
maxLengthOf' n = failureUnless' ((<=n) . length)

-- | Like 'notEmpty' but uses /Unit/ as the 'ValidationRule' error type.
notEmpty' :: Foldable t => e -> ValidationRule (NonEmpty e) (t a)
notEmpty' = failureIf null

-- | Like 'minValueOf' but uses /Unit/ as the 'ValidationRule' error type.
minValueOf' :: Ord a => a -> ValidationRule () a
minValueOf' x = failureUnless' (>=x)

-- | Like 'maxValueOf' but uses /Unit/ as the 'ValidationRule' error type.
maxValueOf' :: Ord a => a -> ValidationRule () a
maxValueOf' x = failureUnless' (<=x)

-- | Like 'onlyContains' but uses /Unit/ as the 'ValidationRule' error type.
onlyContains' :: Foldable t => (a -> Bool) -> ValidationRule () (t a)
onlyContains' x = failureUnless' (all x)

-- | Like 'atleastContains' but uses /Unit/ as the 'ValidationRule' error type.
atleastContains' :: Foldable t => (a -> Bool) -> ValidationRule () (t a)
atleastContains' x = failureUnless' (any x)

-- | Like 'mustContain' but uses /Unit/ as the 'ValidationRule' error type.
mustContain' :: (Foldable t, Eq a) => a -> ValidationRule () (t a)
mustContain' x = failureUnless' (elem x)

---------------------------------------------------------------------
-- Negating 'ValidationRule'
---------------------------------------------------------------------

-- | Build a rule that succeeds if given rule fails and vice versa.
negateRule :: e -> ValidationRule e1 a -> ValidationRule e a
negateRule err (ValidationRule rule) = vrule $ \x -> case rule x of
    Failure _ -> Success ()
    Success _ -> Failure err

-- | Like 'negateRule' but uses /Unit/ as the 'ValidationRule' error type.
negateRule' :: ValidationRule e a -> ValidationRule () a
negateRule' (ValidationRule rule) = vrule $ \x -> case rule x of
    Failure _ -> Success ()
    Success _ -> Failure ()

---------------------------------------------------------------------
-- Combining 'ValidationRule's
---------------------------------------------------------------------

-- | A synonym for 'orElse'. Satisfies associativity law and hence forms a semigroup.
infixr 5 </>

(</>) :: Semigroup e => ValidationRule e a -> ValidationRule e a -> ValidationRule e a
ValidationRule rule1 </> ValidationRule rule2 = vrule $ liftA2 (<>) rule1 rule2

infixr 5 `orElse`

{- | Build a rule that /succeeds/ if __either__ of the given rules succeed. If both fail, the errors are combined.

prop> rule1 `orElse` (rule2 `orElse` rule3) = (rule1 `orElse` rule2) `orElse` rule3
prop> falseRule e `orElse` rule = rule
prop> rule `orElse` falseRule e = rule
-}
orElse :: Semigroup e => ValidationRule e a -> ValidationRule e a -> ValidationRule e a
orElse = (</>)

{- | A 'ValidationRule' that always fails with supplied error. This is the identity of 'orElse' (i.e '(</>)').

prop> falseRule `orElse` rule = rule
prop> rule `orElse` falseRule = rule
-}
falseRule :: Monoid e => ValidationRule e a
falseRule = vrule $ const $ Failure mempty

infixr 6 `andAlso`

{- | Build a rule that /only succeeds/ if __both__ of the given rules succeed. The very first failure is yielded.

This is the same as the semigroup operation (i.e '(<>)') on 'ValidationRule'.

prop> rule1 `andAlso` (rule2 `andAlso` rule3) = (rule1 `andAlso` rule2) `andAlso` rule3
prop> mempty `andAlso` rule = rule
prop> rule `andAlso` mempty = rule
-}
andAlso :: ValidationRule e a -> ValidationRule e a -> ValidationRule e a
andAlso = (<>)

{- | Build a rule that /succeeds/ if __any__ of the given rules succeed. If all fail, the errors are combined.

prop> satisfyAny = foldl1 orElse
prop> satisfyAny = foldr1 orElse
prop> satisfyAny = foldl orElse falseRule
prop> satisfyAny = foldr orElse falseRule
-}
satisfyAny :: (Foldable t, Semigroup e) => t (ValidationRule e a) -> ValidationRule e a
satisfyAny = foldr1 (</>)

{- | Build a rule that /only succeeds/ if __all__ of the given rules succeed. The very first failure is yielded.

prop> satisfyAll = fold
prop> satisfyAll = foldl1 andAlso
prop> satisfyAll = foldr1 andAlso
prop> satisfyAll = foldl andAlso mempty
prop> satisfyAll = foldr andAlso mempty
-}
satisfyAll :: Foldable t => t (ValidationRule e a) -> ValidationRule e a
satisfyAll = fold

-- | Utility to convert a regular predicate function to a 'ValidationRule'. __INTERNAL__
predToRule :: (a -> Bool) -> e -> ValidationRule e a
predToRule predc err = vrule $ \x -> if predc x
    then Success ()
    else Failure err
