{-# LANGUAGE Safe #-}

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
    , lengthAbove
    , lengthBelow
    , lengthWithin
    , maxLengthOf
    , maxValueOf
    , minLengthOf
    , minValueOf
    , mustBe
    , mustContain
    , notEmpty
    , ofLength
    , onlyContains
    , valueAbove
    , valueBelow
    , valueWithin
      -- * Common derivates of primitive /Unit/ combinators
    , atleastContains'
    , lengthAbove'
    , lengthBelow'
    , lengthWithin'
    , maxLengthOf'
    , maxValueOf'
    , minLengthOf'
    , minValueOf'
    , mustBe'
    , mustContain'
    , notEmpty'
    , ofLength'
    , onlyContains'
    , valueAbove'
    , valueBelow'
    , valueWithin'
      -- * Type specific 'ValidationRule's
    , optionally
    ) where

import Control.Applicative (Applicative (liftA2))
import Data.Bool           (bool)
import Data.Foldable       (Foldable (fold))
import Data.Ix             (Ix (inRange))
import Data.List.NonEmpty  (NonEmpty)

import Valida.Utils          (neSingleton)
import Valida.Validation     (Validation (..), validationConst)
import Valida.ValidationRule (ValidationRule (..), vrule)

---------------------------------------------------------------------
-- Primitive 'NonEmpty' combinators
---------------------------------------------------------------------

{- | Build a rule that /fails/ with given error __if the given rule succeeds__.

prop> failureIf predc = failureUnless (not . predc)

==== __Examples__

>>> runValidator (validate (failureIf (>0) "Positive")) 5
Failure ("Positive" :| [])
>>> runValidator (validate (failureIf (>0) "Positive")) 0
Success 0
>>> runValidator (validate (failureIf (>0) "Positive")) (-1)
Success (-1)
-}
failureIf :: (a -> Bool) -> e -> ValidationRule (NonEmpty e) a
failureIf predc = predToRule (not . predc) . neSingleton

{- | Build a rule that /fails/ with given error __unless the given rule succeeds__.

prop> failureUnless predc = failureIf (not . predc)

==== __Examples__

>>> runValidator (validate (failureUnless (>0) "NonPositive")) 5
Success 5
>>> runValidator (validate (failureUnless (>0) "NonPositive")) 0
Failure ("NonPositive" :| [])
>>> runValidator (validate (failureUnless (>0) "NonPositive")) (-1)
Failure ("NonPositive" :| [])
-}
failureUnless :: (a -> Bool) -> e -> ValidationRule (NonEmpty e) a
failureUnless predc = predToRule predc . neSingleton

---------------------------------------------------------------------
-- Primitive /Unit/ combinators
---------------------------------------------------------------------

{- | Like 'failureIf' but uses /Unit/ as the 'ValidationRule' error type.

prop> failureIf' predc = failureUnless' (not . predc)
prop> label (const (err :| [])) (failureIf' predc) = failureIf predc err

==== __Examples__

>>> runValidator (validate (failureIf' (>0))) 5
Failure ()
>>> runValidator (validate (failureIf' (>0))) 0
Success 0
>>> runValidator (validate (failureIf' (>0))) (-1)
Success (-1)
-}
failureIf' :: (a -> Bool) -> ValidationRule () a
failureIf' = flip predToRule () . (not .)

{- | Like 'failureUnless' but uses /Unit/ as the 'ValidationRule' error type.

prop> failureUnless' predc = failureIf' (not . predc)
prop> label (const (err :| [])) (failureUnless' predc) = failureUnless predc err

==== __Examples__

>>> runValidator (validate (failureUnless' (>0))) 5
Success 5
>>> runValidator (validate (failureUnless' (>0))) 0
Failure ()
>>> runValidator (validate (failureUnless' (>0))) (-1)
Failure ()
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
{-# INLINABLE mustBe #-}

{- | Build an equality rule for length.

prop> ofLength x = failureUnless ((==x) . length)
-}
ofLength :: Foldable t => Int -> e -> ValidationRule (NonEmpty e) (t a)
ofLength n = failureUnless $ (==n) . length
{-# INLINABLE ofLength #-}
{-# SPECIALIZE ofLength :: Int -> e -> ValidationRule (NonEmpty e) [a] #-}

{- | Build a minimum length (inclusive) rule.

prop> minLengthOf x = failureUnless ((>=n) . length)
-}
minLengthOf :: Foldable t => Int -> e -> ValidationRule (NonEmpty e) (t a)
minLengthOf n = failureUnless $ (>=n) . length
{-# INLINABLE minLengthOf #-}
{-# SPECIALIZE minLengthOf :: Int -> e -> ValidationRule (NonEmpty e) [a] #-}

{- | Build a maximum length (inclusive) rule.

prop> maxLengthOf n = failureUnless ((<=n) . length)
-}
maxLengthOf :: Foldable t => Int -> e -> ValidationRule (NonEmpty e) (t a)
maxLengthOf n = failureUnless $ (<=n) . length
{-# INLINABLE maxLengthOf #-}
{-# SPECIALIZE maxLengthOf :: Int -> e -> ValidationRule (NonEmpty e) [a] #-}

{- | Build a minimum length (inclusive) rule.

prop> lengthAbove x = minLengthOf (x + 1)
prop> lengthAbove x = failureUnless ((>n) . length)
-}
lengthAbove :: Foldable t => Int -> e -> ValidationRule (NonEmpty e) (t a)
lengthAbove n = failureUnless $ (>n) . length
{-# INLINABLE lengthAbove #-}
{-# SPECIALIZE lengthAbove :: Int -> e -> ValidationRule (NonEmpty e) [a] #-}

{- | Build a maximum length (inclusive) rule.

prop> lengthBelow x = maxLengthOf (x - 1)
prop> lengthBelow x = failureUnless ((<n) . length)
-}
lengthBelow :: Foldable t => Int -> e -> ValidationRule (NonEmpty e) (t a)
lengthBelow n = failureUnless $ (<n) . length
{-# INLINABLE lengthBelow #-}
{-# SPECIALIZE lengthBelow :: Int -> e -> ValidationRule (NonEmpty e) [a] #-}

{- | Build a maximum length rule.

prop> notEmpty = minLengthOf 1
prop> notEmpty = failureIf null
-}
notEmpty :: Foldable t => e -> ValidationRule (NonEmpty e) (t a)
notEmpty = failureIf null
{-# INLINABLE notEmpty #-}
{-# SPECIALIZE notEmpty :: e -> ValidationRule (NonEmpty e) [a] #-}

{- | Build an 'inRange' rule for length.

prop> lengthWithin (min, max) = minLengthOf min `andAlso` maxLengthOf max
prop> lengthWithin r = failureUnless (inRange r . length)
-}
lengthWithin :: Foldable t => (Int, Int) -> e -> ValidationRule (NonEmpty e) (t a)
lengthWithin r = failureUnless $ inRange r . length
{-# INLINABLE lengthWithin #-}
{-# SPECIALIZE lengthWithin :: (Int, Int) -> e -> ValidationRule (NonEmpty e) [a] #-}

{- | Build a minimum value (inclusive) rule.

prop> minValueOf x = failureUnless (>=x)
-}
minValueOf :: Ord a => a -> e -> ValidationRule (NonEmpty e) a
minValueOf x = failureUnless (>=x)
{-# INLINABLE minValueOf #-}

{- | Build a maximum value (inclusive) rule.

prop> maxValueOf x = failureUnless (<=x)
-}
maxValueOf :: Ord a => a -> e -> ValidationRule (NonEmpty e) a
maxValueOf x = failureUnless (<=x)
{-# INLINABLE maxValueOf #-}

{- | Build a minimum value (exclusive) rule.

prop> valueAbove x = minValueOf (x + 1)
prop> valueAbove x = failureUnless (>x)
-}
valueAbove :: Ord a => a -> e -> ValidationRule (NonEmpty e) a
valueAbove n = failureUnless (>n)
{-# INLINABLE valueAbove #-}

{- | Build a maximum value (exclusive) rule.

prop> valueBelow x = minValueOf (x - 1)
prop> valueBelow x = failureUnless (<x)
-}
valueBelow :: Ord a => a -> e -> ValidationRule (NonEmpty e) a
valueBelow n = failureUnless (<n)
{-# INLINABLE valueBelow #-}

{- | Build an 'inRange' rule for value.

prop> valueWithin (min, max) = minValueOf min `andAlso` maxValueOf max
prop> valueWithin r = failureUnless (inRange r)
-}
valueWithin :: Ix a => (a, a) -> e -> ValidationRule (NonEmpty e) a
valueWithin r = failureUnless $ inRange r
{-# INLINABLE valueWithin #-}
{-# SPECIALIZE valueWithin :: (Int, Int) -> e -> ValidationRule (NonEmpty e) Int #-}

{- | Build an 'all' rule.

prop> onlyContains x = failureUnless (all x)
-}
onlyContains :: Foldable t => (a -> Bool) -> e -> ValidationRule (NonEmpty e) (t a)
onlyContains x = failureUnless $ all x
{-# INLINABLE onlyContains #-}
{-# SPECIALIZE onlyContains :: (a -> Bool) -> e -> ValidationRule (NonEmpty e) [a] #-}

{- | Build an 'any' rule.

prop> atleastContains x = failureUnless (any x)
-}
atleastContains :: Foldable t => (a -> Bool) -> e -> ValidationRule (NonEmpty e) (t a)
atleastContains x = failureUnless $ any x
{-# INLINABLE atleastContains #-}
{-# SPECIALIZE atleastContains :: (a -> Bool) -> e -> ValidationRule (NonEmpty e) [a] #-}

{- | Build an 'elem' rule.

prop> mustContain x = atleastContains (==x)

prop> mustContain x = failureUnless (elem x)
-}
mustContain :: (Foldable t, Eq a) => a -> e -> ValidationRule (NonEmpty e) (t a)
mustContain x = failureUnless $ elem x
{-# INLINABLE mustContain #-}
{-# SPECIALIZE mustContain :: Eq a => a -> e -> ValidationRule (NonEmpty e) [a] #-}

---------------------------------------------------------------------
-- Common derivates of primitive /Unit/ combinators
---------------------------------------------------------------------

-- | Like 'mustBe' but uses /Unit/ as the 'ValidationRule' error type.
mustBe' :: Eq a => a -> ValidationRule () a
mustBe' x = failureUnless' (==x)
{-# INLINABLE mustBe' #-}

-- | Like 'ofLength' but uses /Unit/ as the 'ValidationRule' error type.
ofLength' :: Foldable t => Int -> ValidationRule () (t a)
ofLength' n = failureUnless' $ (==n) . length
{-# INLINABLE ofLength' #-}
{-# SPECIALIZE ofLength' :: Int -> ValidationRule () [a] #-}

-- | Like 'minLengthOf' but uses /Unit/ as the 'ValidationRule' error type.
minLengthOf' :: Foldable t => Int -> ValidationRule () (t a)
minLengthOf' n = failureUnless' $ (>=n) . length
{-# INLINABLE minLengthOf' #-}
{-# SPECIALIZE minLengthOf' :: Int -> ValidationRule () [a] #-}

-- | Like 'maxLengthOf' but uses /Unit/ as the 'ValidationRule' error type.
maxLengthOf' :: Foldable t => Int -> ValidationRule () (t a)
maxLengthOf' n = failureUnless' $ (<=n) . length
{-# INLINABLE maxLengthOf' #-}
{-# SPECIALIZE maxLengthOf' :: Int -> ValidationRule () [a] #-}

-- | Like 'lengthAbove' but uses /Unit/ as the 'ValidationRule' error type.
lengthAbove' :: Foldable t => Int -> ValidationRule () (t a)
lengthAbove' n = failureUnless' $ (>n) . length
{-# INLINABLE lengthAbove' #-}
{-# SPECIALIZE lengthAbove' :: Int -> ValidationRule () [a] #-}

-- | Like 'lengthBelow' but uses /Unit/ as the 'ValidationRule' error type.
lengthBelow' :: Foldable t => Int -> ValidationRule () (t a)
lengthBelow' n = failureUnless' $ (<n) . length
{-# INLINABLE lengthBelow' #-}
{-# SPECIALIZE lengthBelow' :: Int -> ValidationRule () [a] #-}

-- | Like 'notEmpty' but uses /Unit/ as the 'ValidationRule' error type.
notEmpty' :: Foldable t => ValidationRule () (t a)
notEmpty' = failureIf' null
{-# INLINABLE notEmpty' #-}
{-# SPECIALIZE notEmpty' :: ValidationRule () [a] #-}

-- | Like 'lengthWithin' but uses /Unit/ as the 'ValidationRule' error type.
lengthWithin' :: Foldable t => (Int, Int) -> ValidationRule () (t a)
lengthWithin' r = failureUnless' $ inRange r . length
{-# INLINABLE lengthWithin' #-}
{-# SPECIALIZE ofLength' :: Int -> ValidationRule () [a] #-}

-- | Like 'minValueOf' but uses /Unit/ as the 'ValidationRule' error type.
minValueOf' :: Ord a => a -> ValidationRule () a
minValueOf' x = failureUnless' (>=x)
{-# INLINABLE minValueOf' #-}

-- | Like 'maxValueOf' but uses /Unit/ as the 'ValidationRule' error type.
maxValueOf' :: Ord a => a -> ValidationRule () a
maxValueOf' x = failureUnless' (<=x)
{-# INLINABLE maxValueOf' #-}

-- | Like 'valueAbove' but uses /Unit/ as the 'ValidationRule' error type.
valueAbove' :: Ord a => a -> ValidationRule () a
valueAbove' n = failureUnless' (>n)
{-# INLINABLE valueAbove' #-}

-- | Like 'valueBelow' but uses /Unit/ as the 'ValidationRule' error type.
valueBelow' :: Ord a => a -> ValidationRule () a
valueBelow' n = failureUnless' (<n)
{-# INLINABLE valueBelow' #-}

-- | Like 'valueWithin' but uses /Unit/ as the 'ValidationRule' error type.
valueWithin' :: Ix a => (a, a) -> ValidationRule () a
valueWithin' r = failureUnless' $ inRange r
{-# INLINABLE valueWithin' #-}
{-# SPECIALIZE valueWithin' :: (Int, Int) -> ValidationRule () Int #-}

-- | Like 'onlyContains' but uses /Unit/ as the 'ValidationRule' error type.
onlyContains' :: Foldable t => (a -> Bool) -> ValidationRule () (t a)
onlyContains' x = failureUnless' $ all x
{-# INLINABLE onlyContains' #-}
{-# SPECIALIZE onlyContains' :: (a -> Bool) -> ValidationRule () [a] #-}

-- | Like 'atleastContains' but uses /Unit/ as the 'ValidationRule' error type.
atleastContains' :: Foldable t => (a -> Bool) -> ValidationRule () (t a)
atleastContains' x = failureUnless' $ any x
{-# INLINABLE atleastContains' #-}
{-# SPECIALIZE atleastContains' :: (a -> Bool) -> ValidationRule () [a] #-}

-- | Like 'mustContain' but uses /Unit/ as the 'ValidationRule' error type.
mustContain' :: (Foldable t, Eq a) => a -> ValidationRule () (t a)
mustContain' x = failureUnless' $ elem x
{-# INLINABLE mustContain' #-}
{-# SPECIALIZE mustContain' :: Eq a => a -> ValidationRule () [a] #-}

---------------------------------------------------------------------
-- Negating 'ValidationRule'
---------------------------------------------------------------------

{- | Build a rule that succeeds if given rule fails and vice versa.

==== __Examples__

>>> let rule = negateRule "NonPositive" (failureIf (>0) "Positive")
>>> runValidator (validate rule) 5
Success 5
>>> runValidator (validate rule) 0
Failure "NonPositive"
>>> runValidator (validate rule) (-1)
Failure "NonPositive"
-}
negateRule :: e -> ValidationRule e1 a -> ValidationRule e a
negateRule err (ValidationRule rule) = vrule $ validationConst (Success ()) (Failure err) . rule

-- | Like 'negateRule' but uses /Unit/ as the 'ValidationRule' error type.
negateRule' :: ValidationRule e a -> ValidationRule () a
negateRule' (ValidationRule rule) = vrule $ ($ ()) . validationConst Success Failure . rule

---------------------------------------------------------------------
-- Combining 'ValidationRule's
---------------------------------------------------------------------

-- | A synonym for 'orElse'. Satisfies associativity law and hence forms a semigroup.
infixr 5 </>

(</>) :: Semigroup e => ValidationRule e a -> ValidationRule e a -> ValidationRule e a
ValidationRule rule1 </> ValidationRule rule2 = vrule $ liftA2 (<>) rule1 rule2
{-# INLINABLE (</>)  #-}
{-# SPECIALIZE (</>)
    :: ValidationRule (NonEmpty err) a
    -> ValidationRule (NonEmpty err) a
    -> ValidationRule (NonEmpty err) a #-}
{-# SPECIALIZE (</>) :: ValidationRule () a -> ValidationRule () a -> ValidationRule () a #-}
{-# SPECIALIZE (</>) :: ValidationRule [err] a -> ValidationRule [err] a -> ValidationRule [err] a #-}

{- | Build a rule that /succeeds/ if __either__ of the given rules succeed. If both fail, the errors are combined.

prop> rule1 `orElse` (rule2 `orElse` rule3) = (rule1 `orElse` rule2) `orElse` rule3
prop> falseRule e `orElse` rule = rule
prop> rule `orElse` falseRule e = rule

==== __Examples__

>>> let rule = failureIf (>0) "Positive" `orElse` failureIf even "Even"
>>> runValidator (validate rule) 5
Success 5
>>> runValidator (validate rule) 4
Failure ("Positive" :| ["Even"])
>>> runValidator (validate rule) 0
Success 0
>>> runValidator (validate rule) (-1)
Success (-1)
-}
orElse :: Semigroup e => ValidationRule e a -> ValidationRule e a -> ValidationRule e a
orElse = (</>)
{-# INLINABLE orElse #-}

{- | A 'ValidationRule' that always fails with supplied error. This is the identity of 'orElse' (i.e '(</>)').

prop> falseRule `orElse` rule = rule
prop> rule `orElse` falseRule = rule

==== __Examples__

>>> runValidator (validate falseRule) 42
Failure ()
-}
falseRule :: Monoid e => ValidationRule e a
falseRule = vrule $ const $ Failure mempty
{-# INLINABLE falseRule #-}

{- | Build a rule that /only succeeds/ if __both__ of the given rules succeed. The very first failure is yielded.

This is the same as the semigroup operation (i.e '(<>)') on 'ValidationRule'.

prop> rule1 `andAlso` (rule2 `andAlso` rule3) = (rule1 `andAlso` rule2) `andAlso` rule3
prop> mempty `andAlso` rule = rule
prop> rule `andAlso` mempty = rule

==== __Examples__

>>> let rule = failureIf (>0) "Positive" `andAlso` failureIf even "Even"
>>> runValidator (validate rule) 5
Failure ("Positive" :| [])
>>> runValidator (validate rule) (-2)
Failure ("Even" :| [])
>>> runValidator (validate rule) (-1)
Success (-1)
-}
andAlso :: ValidationRule e a -> ValidationRule e a -> ValidationRule e a
andAlso = (<>)
{-# INLINABLE andAlso #-}

{- | Build a rule that /succeeds/ if __any__ of the given rules succeed. If all fail, the errors are combined.

prop> satisfyAny = foldl1 orElse
prop> satisfyAny = foldr1 orElse
prop> satisfyAny = foldl orElse falseRule
prop> satisfyAny = foldr orElse falseRule
-}
satisfyAny :: (Foldable t, Semigroup e) => t (ValidationRule e a) -> ValidationRule e a
satisfyAny = foldr1 (</>)
{-# INLINABLE satisfyAny #-}
{-# SPECIALIZE satisfyAny :: [ValidationRule (NonEmpty err) a] -> ValidationRule (NonEmpty err) a #-}
{-# SPECIALIZE satisfyAny :: [ValidationRule () a] -> ValidationRule () a #-}
{-# SPECIALIZE satisfyAny :: [ValidationRule [err] a] -> ValidationRule [err] a #-}

{- | Build a rule that /only succeeds/ if __all__ of the given rules succeed. The very first failure is yielded.

prop> satisfyAll = fold
prop> satisfyAll = foldl1 andAlso
prop> satisfyAll = foldr1 andAlso
prop> satisfyAll = foldl andAlso mempty
prop> satisfyAll = foldr andAlso mempty
-}
satisfyAll :: Foldable t => t (ValidationRule e a) -> ValidationRule e a
satisfyAll = fold
{-# INLINABLE satisfyAll #-}
{-# SPECIALIZE satisfyAll :: [ValidationRule e a] -> ValidationRule e a #-}

---------------------------------------------------------------------
-- Type specific 'ValidationRule's
---------------------------------------------------------------------

{- | Build a rule that runs given rule only if input is 'Just'.

Yields 'Success' when input is 'Nothing.

==== __Examples__

>>> runValidator (validate (optionally (failureIf even "Even"))) (Just 5)
Success (Just 5)
>>> runValidator (validate (optionally (failureIf even "Even"))) (Just 6)
Failure ("Even" :| [])
>>> runValidator (validate (optionally (failureIf even "Even"))) Nothing
Success Nothing
-}
optionally :: ValidationRule e a -> ValidationRule e (Maybe a)
optionally (ValidationRule rule) = vrule $ maybe (Success ()) rule

-- | Utility to convert a regular predicate function to a 'ValidationRule'. __INTERNAL__
predToRule :: (a -> Bool) -> e -> ValidationRule e a
predToRule predc err = vrule $ bool (Failure err) (Success ()) . predc
