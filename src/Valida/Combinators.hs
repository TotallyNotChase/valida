{-# LANGUAGE Safe #-}

{- |
Module      : Valida.Combinators
Description : Combinators and utilities for building and combining 'Validator's
Copyright   : (c) TotallyNotChase, 2021
License     : MIT
Maintainer  : totallynotchase42@gmail.com
Stability   : Stable
Portability : Portable

This module is re-exported by "Valida". You probably don't need to import this.

This module exports the primitive, as well as utility, 'Validator' combinators.
As well as the 'orElse', 'andAlso', 'satisfyAny', and 'satisfyAll' functions, and some more utilities.

__Note__: All the primitive combinators and derivative combinators use the same type for @inp@ and @a@.
In those cases - upon successful validation, the input itself, wrapped in 'Success', is returned.
-}

module Valida.Combinators
    ( -- * Primitive 'NonEmpty' combinators
      failureIf
    , failureUnless
      -- * Primitive /Unit/ combinators
    , failureIf'
    , failureUnless'
      -- * Negating 'Validator'
    , negateV
    , negateV'
      -- * Combining 'Validator's
    , andAlso
    , orElse
      --
    , failV
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
      -- * Type specific 'Validator's
    , optionally
    ) where

import Data.Ix            (Ix (inRange))
import Data.List.NonEmpty (NonEmpty)

import Valida.Utils      (neSingleton)
import Valida.Validation (Validation (..), validationConst)
import Valida.Validator  (Validator (Validator))

---------------------------------------------------------------------
-- Primitive 'NonEmpty' combinators
---------------------------------------------------------------------

{- | Build a rule that /fails/ with given error __if the given predicate succeeds__.

@failureIf predc = 'failureUnless' ('not' . predc)@

==== __Examples__

>>> runValidator (failureIf (>0) "Positive") 5
Failure ("Positive" :| [])
>>> runValidator (failureIf (>0) "Positive") 0
Success 0
>>> runValidator (failureIf (>0) "Positive") (-1)
Success (-1)
-}
failureIf :: (a -> Bool) -> e -> Validator (NonEmpty e) a a
failureIf predc = validatorFrom (not . predc) . neSingleton

{- | Build a rule that /fails/ with given error __unless the given predicate succeeds__.

@failureUnless predc = 'failureIf' ('not' . predc)@

==== __Examples__

>>> runValidator (failureUnless (>0) "NonPositive") 5
Success 5
>>> runValidator (failureUnless (>0) "NonPositive") 0
Failure ("NonPositive" :| [])
>>> runValidator (failureUnless (>0) "NonPositive") (-1)
Failure ("NonPositive" :| [])
-}
failureUnless :: (a -> Bool) -> e -> Validator (NonEmpty e) a a
failureUnless predc = validatorFrom predc . neSingleton

---------------------------------------------------------------------
-- Primitive /Unit/ combinators
---------------------------------------------------------------------

{- | Like 'failureIf' but uses /Unit/ as the 'Validator' error type.

@failureIf' predc = 'failureUnless'' ('not' . predc)@

@label ('const' (err :| [])) (failureIf' predc) = 'failureIf' predc err@

==== __Examples__

>>> runValidator (failureIf' (>0)) 5
Failure ()
>>> runValidator (failureIf' (>0)) 0
Success 0
>>> runValidator (failureIf' (>0)) (-1)
Success (-1)
-}
failureIf' :: (a -> Bool) -> Validator () a a
failureIf' predc = validatorFrom (not . predc) ()

{- | Like 'failureUnless' but uses /Unit/ as the 'Validator' error type.

@failureUnless' predc = 'failureIf'' ('not' . predc)@

@label ('const' (err :| [])) (failureUnless' predc) = 'failureUnless' predc err@

==== __Examples__

>>> runValidator (failureUnless' (>0)) 5
Success 5
>>> runValidator (failureUnless' (>0)) 0
Failure ()
>>> runValidator (failureUnless' (>0)) (-1)
Failure ()
-}
failureUnless' :: (a -> Bool) -> Validator () a a
failureUnless' = flip validatorFrom ()

---------------------------------------------------------------------
-- Common derivates of primitive 'NonEmpty' combinators
---------------------------------------------------------------------

{- | Build an equality rule for value.

@mustBe x = 'failureUnless' (==x)@
-}
mustBe :: Eq a => a -> e -> Validator (NonEmpty e) a a
mustBe x = failureUnless (==x)
{-# INLINABLE mustBe #-}

{- | Build an equality rule for length.

@ofLength x = 'failureUnless' ((==x) . 'length')@
-}
ofLength :: Foldable t => Int -> e -> Validator (NonEmpty e) (t a) (t a)
ofLength n = failureUnless $ (==n) . length
{-# INLINABLE ofLength #-}
{-# SPECIALIZE ofLength :: Int -> e -> Validator (NonEmpty e) [a] [a] #-}

{- | Build a minimum length (inclusive) rule.

@minLengthOf x = 'failureUnless' ((>=n) . 'length')@
-}
minLengthOf :: Foldable t => Int -> e -> Validator (NonEmpty e) (t a) (t a)
minLengthOf n = failureUnless $ (>=n) . length
{-# INLINABLE minLengthOf #-}
{-# SPECIALIZE minLengthOf :: Int -> e -> Validator (NonEmpty e) [a] [a] #-}

{- | Build a maximum length (inclusive) rule.

@maxLengthOf n = 'failureUnless' ((<=n) . 'length')@
-}
maxLengthOf :: Foldable t => Int -> e -> Validator (NonEmpty e) (t a) (t a)
maxLengthOf n = failureUnless $ (<=n) . length
{-# INLINABLE maxLengthOf #-}
{-# SPECIALIZE maxLengthOf :: Int -> e -> Validator (NonEmpty e) [a] [a] #-}

{- | Build a minimum length (inclusive) rule.

@lengthAbove x = 'minLengthOf' (x + 1)@

@lengthAbove x = 'failureUnless' ((>n) . 'length')@
-}
lengthAbove :: Foldable t => Int -> e -> Validator (NonEmpty e) (t a) (t a)
lengthAbove n = failureUnless $ (>n) . length
{-# INLINABLE lengthAbove #-}
{-# SPECIALIZE lengthAbove :: Int -> e -> Validator (NonEmpty e) [a] [a] #-}

{- | Build a maximum length (inclusive) rule.

@lengthBelow x = 'maxLengthOf' (x - 1)@

@lengthBelow x = 'failureUnless' ((<n) . 'length')@
-}
lengthBelow :: Foldable t => Int -> e -> Validator (NonEmpty e) (t a) (t a)
lengthBelow n = failureUnless $ (<n) . length
{-# INLINABLE lengthBelow #-}
{-# SPECIALIZE lengthBelow :: Int -> e -> Validator (NonEmpty e) [a] [a] #-}

{- | Build a maximum length rule.

@notEmpty = 'minLengthOf' 1@

@notEmpty = 'failureIf' 'null'@
-}
notEmpty :: Foldable t => e -> Validator (NonEmpty e) (t a) (t a)
notEmpty = failureIf null
{-# INLINABLE notEmpty #-}
{-# SPECIALIZE notEmpty :: e -> Validator (NonEmpty e) [a] [a] #-}

{- | Build an 'inRange' rule for length.

@lengthWithin (min, max) = 'minLengthOf' min `'andAlso'` 'maxLengthOf' max@

@lengthWithin r = 'failureUnless' ('inRange' r . 'length')@
-}
lengthWithin :: Foldable t => (Int, Int) -> e -> Validator (NonEmpty e) (t a) (t a)
lengthWithin r = failureUnless $ inRange r . length
{-# INLINABLE lengthWithin #-}
{-# SPECIALIZE lengthWithin :: (Int, Int) -> e -> Validator (NonEmpty e) [a] [a] #-}

{- | Build a minimum value (inclusive) rule.

@minValueOf x = 'failureUnless' (>=x)@
-}
minValueOf :: Ord a => a -> e -> Validator (NonEmpty e) a a
minValueOf x = failureUnless (>=x)
{-# INLINABLE minValueOf #-}

{- | Build a maximum value (inclusive) rule.

@maxValueOf x = 'failureUnless' (<=x)@
-}
maxValueOf :: Ord a => a -> e -> Validator (NonEmpty e) a a
maxValueOf x = failureUnless (<=x)
{-# INLINABLE maxValueOf #-}

{- | Build a minimum value (exclusive) rule.

@valueAbove x = 'minValueOf' (x + 1)@

@valueAbove x = 'failureUnless' (>x)@
-}
valueAbove :: Ord a => a -> e -> Validator (NonEmpty e) a a
valueAbove n = failureUnless (>n)
{-# INLINABLE valueAbove #-}

{- | Build a maximum value (exclusive) rule.

@valueBelow x = 'minValueOf' (x - 1)@

@valueBelow x = 'failureUnless' (<x)@
-}
valueBelow :: Ord a => a -> e -> Validator (NonEmpty e) a a
valueBelow n = failureUnless (<n)
{-# INLINABLE valueBelow #-}

{- | Build an 'inRange' rule for value.

@valueWithin (m, n) = 'minValueOf' m `'andAlso'` 'maxValueOf' n@

@valueWithin (m, n) = 'failureUnless' (\x -> m <= x && x <= n)@
-}
valueWithin :: Ord a => (a, a) -> e -> Validator (NonEmpty e) a a
valueWithin (m, n) = failureUnless $ \x -> m <= x && x <= n
{-# INLINABLE valueWithin #-}
{-# SPECIALIZE valueWithin :: (Int, Int) -> e -> Validator (NonEmpty e) Int Int #-}

{- | Build an 'all' rule.

@onlyContains x = 'failureUnless' ('all' x)@
-}
onlyContains :: Foldable t => (a -> Bool) -> e -> Validator (NonEmpty e) (t a) (t a)
onlyContains x = failureUnless $ all x
{-# INLINABLE onlyContains #-}
{-# SPECIALIZE onlyContains :: (a -> Bool) -> e -> Validator (NonEmpty e) [a] [a] #-}

{- | Build an 'any' rule.

@atleastContains x = 'failureUnless' ('any' x)@
-}
atleastContains :: Foldable t => (a -> Bool) -> e -> Validator (NonEmpty e) (t a) (t a)
atleastContains x = failureUnless $ any x
{-# INLINABLE atleastContains #-}
{-# SPECIALIZE atleastContains :: (a -> Bool) -> e -> Validator (NonEmpty e) [a] [a] #-}

{- | Build an 'elem' rule.

@mustContain x = 'atleastContains' (==x)@

@mustContain x = 'failureUnless' ('elem' x)@
-}
mustContain :: (Foldable t, Eq a) => a -> e -> Validator (NonEmpty e) (t a) (t a)
mustContain x = failureUnless $ elem x
{-# INLINABLE mustContain #-}
{-# SPECIALIZE mustContain :: Eq a => a -> e -> Validator (NonEmpty e) [a] [a] #-}

---------------------------------------------------------------------
-- Common derivates of primitive /Unit/ combinators
---------------------------------------------------------------------

-- | Like 'mustBe' but uses /Unit/ as the 'Validator' error type.
mustBe' :: Eq a => a -> Validator () a a
mustBe' x = failureUnless' (==x)
{-# INLINABLE mustBe' #-}

-- | Like 'ofLength' but uses /Unit/ as the 'Validator' error type.
ofLength' :: Foldable t => Int -> Validator () (t a) (t a)
ofLength' n = failureUnless' $ (==n) . length
{-# INLINABLE ofLength' #-}
{-# SPECIALIZE ofLength' :: Int -> Validator () [a] [a] #-}

-- | Like 'minLengthOf' but uses /Unit/ as the 'Validator' error type.
minLengthOf' :: Foldable t => Int -> Validator () (t a) (t a)
minLengthOf' n = failureUnless' $ (>=n) . length
{-# INLINABLE minLengthOf' #-}
{-# SPECIALIZE minLengthOf' :: Int -> Validator () [a] [a] #-}

-- | Like 'maxLengthOf' but uses /Unit/ as the 'Validator' error type.
maxLengthOf' :: Foldable t => Int -> Validator () (t a) (t a)
maxLengthOf' n = failureUnless' $ (<=n) . length
{-# INLINABLE maxLengthOf' #-}
{-# SPECIALIZE maxLengthOf' :: Int -> Validator () [a] [a] #-}

-- | Like 'lengthAbove' but uses /Unit/ as the 'Validator' error type.
lengthAbove' :: Foldable t => Int -> Validator () (t a) (t a)
lengthAbove' n = failureUnless' $ (>n) . length
{-# INLINABLE lengthAbove' #-}
{-# SPECIALIZE lengthAbove' :: Int -> Validator () [a] [a] #-}

-- | Like 'lengthBelow' but uses /Unit/ as the 'Validator' error type.
lengthBelow' :: Foldable t => Int -> Validator () (t a) (t a)
lengthBelow' n = failureUnless' $ (<n) . length
{-# INLINABLE lengthBelow' #-}
{-# SPECIALIZE lengthBelow' :: Int -> Validator () [a] [a] #-}

-- | Like 'notEmpty' but uses /Unit/ as the 'Validator' error type.
notEmpty' :: Foldable t => Validator () (t a) (t a)
notEmpty' = failureIf' null
{-# INLINABLE notEmpty' #-}
{-# SPECIALIZE notEmpty' :: Validator () [a] [a] #-}

-- | Like 'lengthWithin' but uses /Unit/ as the 'Validator' error type.
lengthWithin' :: Foldable t => (Int, Int) -> Validator () (t a) (t a)
lengthWithin' r = failureUnless' $ inRange r . length
{-# INLINABLE lengthWithin' #-}
{-# SPECIALIZE ofLength' :: Int -> Validator () [a] [a] #-}

-- | Like 'minValueOf' but uses /Unit/ as the 'Validator' error type.
minValueOf' :: Ord a => a -> Validator () a a
minValueOf' x = failureUnless' (>=x)
{-# INLINABLE minValueOf' #-}

-- | Like 'maxValueOf' but uses /Unit/ as the 'Validator' error type.
maxValueOf' :: Ord a => a -> Validator () a a
maxValueOf' x = failureUnless' (<=x)
{-# INLINABLE maxValueOf' #-}

-- | Like 'valueAbove' but uses /Unit/ as the 'Validator' error type.
valueAbove' :: Ord a => a -> Validator () a a
valueAbove' n = failureUnless' (>n)
{-# INLINABLE valueAbove' #-}

-- | Like 'valueBelow' but uses /Unit/ as the 'Validator' error type.
valueBelow' :: Ord a => a -> Validator () a a
valueBelow' n = failureUnless' (<n)
{-# INLINABLE valueBelow' #-}

-- | Like 'valueWithin' but uses /Unit/ as the 'Validator' error type.
valueWithin' :: Ord a => (a, a) -> Validator () a a
valueWithin' (m, n) = failureUnless' $ \x -> m <= x && x <= n
{-# INLINABLE valueWithin' #-}
{-# SPECIALIZE valueWithin' :: (Int, Int) -> Validator () Int Int #-}

-- | Like 'onlyContains' but uses /Unit/ as the 'Validator' error type.
onlyContains' :: Foldable t => (a -> Bool) -> Validator () (t a) (t a)
onlyContains' x = failureUnless' $ all x
{-# INLINABLE onlyContains' #-}
{-# SPECIALIZE onlyContains' :: (a -> Bool) -> Validator () [a] [a] #-}

-- | Like 'atleastContains' but uses /Unit/ as the 'Validator' error type.
atleastContains' :: Foldable t => (a -> Bool) -> Validator () (t a) (t a)
atleastContains' x = failureUnless' $ any x
{-# INLINABLE atleastContains' #-}
{-# SPECIALIZE atleastContains' :: (a -> Bool) -> Validator () [a] [a] #-}

-- | Like 'mustContain' but uses /Unit/ as the 'Validator' error type.
mustContain' :: (Foldable t, Eq a) => a -> Validator () (t a) (t a)
mustContain' x = failureUnless' $ elem x
{-# INLINABLE mustContain' #-}
{-# SPECIALIZE mustContain' :: Eq a => a -> Validator () [a] [a] #-}

---------------------------------------------------------------------
-- Negating 'Validator'
---------------------------------------------------------------------

{- | Build a validator that succeeds if given validator fails and vice versa.

__Note__: This will set the output of the 'Validator' to be the same as its input.

==== __Examples__

>>> let vald = negateV "NonPositive" (failureIf (>0) "Positive")
>>> runValidator vald 5
Success 5
>>> runValidator vald 0
Failure "NonPositive"
>>> runValidator vald (-1)
Failure "NonPositive"
-}
negateV :: e -> Validator e1 a x -> Validator e a a
negateV err (Validator v) = Validator $ \x -> validationConst (Success x) (Failure err) $ v x

-- | Like 'negateV' but uses /Unit/ as the 'Validator' error type.
negateV' :: Validator e a x -> Validator () a a
negateV' (Validator v) = Validator $ \x -> validationConst (Success x) (Failure ()) $ v x

---------------------------------------------------------------------
-- Combining 'Validator's
---------------------------------------------------------------------

-- | A synonym for 'orElse'. Satisfies associativity law and hence forms a semigroup.
infixr 5 </>

(</>) :: Semigroup e => Validator e inp a -> Validator e inp a -> Validator e inp a
Validator v1 </> Validator v2 = Validator $ v1 <> v2
{-# INLINABLE (</>)  #-}
{-# SPECIALIZE (</>)
    :: Validator (NonEmpty err) inp a
    -> Validator (NonEmpty err) inp a
    -> Validator (NonEmpty err) inp a #-}
{-# SPECIALIZE (</>) :: Validator () inp a -> Validator () inp a -> Validator () inp a #-}
{-# SPECIALIZE (</>) :: Validator [err] inp a -> Validator [err] inp a -> Validator [err] inp a #-}

{- | Build a validator that /succeeds/ if __either__ of the given validators succeed.
The first (left-most) 'Success' is returned. If both fail, the errors are combined.
Other validator /is not used/ if first one succeeds.

@vald1 \`orElse\` (vald2 \`orElse\` vald3) = (vald1 \`orElse\` vald2) \`orElse\` vald3@

@'failV' e \`orElse\` vald = vald@

@vald \`orElse\` 'failV' e = vald@

==== __Examples__

>>> let vald = failureIf (>0) "Positive" `orElse` failureIf even "Even"
>>> runValidator vald 5
Success 5
>>> runValidator vald 4
Failure ("Positive" :| ["Even"])
>>> runValidator vald 0
Success 0
>>> runValidator vald (-1)
Success (-1)
-}
orElse :: Semigroup e => Validator e inp a -> Validator e inp a -> Validator e inp a
orElse = (</>)
{-# INLINABLE orElse #-}

{- | A 'Validator' that always fails with supplied error. This is the identity of 'orElse' (i.e '(</>)').

@failV `'orElse'` vald = vald@

@vald `'orElse'` failV = vald@

==== __Examples__

>>> runValidator (failV :: Validator String Int Int) 42
Failure ""
-}
failV :: Monoid e => Validator e inp a
failV = Validator $ const $ Failure mempty
{-# INLINABLE failV #-}

{- | Build a validator that /only succeeds/ if __both__ of the given validators succeed.
The __first (left-most)__ failure is yielded. If both succeed, the __right-most__ 'Success' result is returned.
Other validator /is not used/ if first one fails.

This is the same as the semigroup operation (i.e '(<>)') on 'Validator'.

@vald1 \`andAlso\` (vald2 \`andAlso\` vald3) = (vald1 \`andAlso\` vald2) \`andAlso\` vald3@

@'mempty' \`andAlso\` vald = vald@

@vald \`andAlso\` 'mempty' = vald@

==== __Examples__

>>> let vald = failureIf (>0) "Positive" `andAlso` failureIf even "Even"
>>> runValidator vald 5
Failure ("Positive" :| [])
>>> runValidator vald (-2)
Failure ("Even" :| [])
>>> runValidator vald (-1)
Success (-1)
-}
andAlso :: Validator e inp a -> Validator e inp a -> Validator e inp a
andAlso = (<>)
{-# INLINABLE andAlso #-}

{- | Build a validator that /succeeds/ if __any__ of the given validators succeed. If all fail, the errors are combined.
The __first (left-most)__ 'Success' is returned. If all fail, the errors are /combined/.
Remaining validators /are not used/ once one succeeds.

@satisfyAny = 'foldl1' 'orElse'@

@satisfyAny = 'foldr1' 'orElse'@

@satisfyAny = 'foldl' 'orElse' 'failV'@

@satisfyAny = 'foldr' 'orElse' 'failV'@
-}
satisfyAny :: (Foldable t, Semigroup e) => t (Validator e inp a) -> Validator e inp a
satisfyAny = foldr1 orElse
{-# INLINABLE satisfyAny #-}
{-# SPECIALIZE satisfyAny :: [Validator (NonEmpty err) inp a] -> Validator (NonEmpty err) inp a #-}
{-# SPECIALIZE satisfyAny :: [Validator () inp a] -> Validator () inp a #-}
{-# SPECIALIZE satisfyAny :: [Validator [err] inp a] -> Validator [err] inp a #-}

{- | Build a validator that /only succeeds/ if __all__ of the given validators succeed.
The __first (left-most)__ failure is yielded. If all succeed, the __right-most__ 'Success' result is returned.
Remaining validators /are not used/ once one fails.

@satisfyAll = 'Data.Foldable.fold'@

@satisfyAll = 'foldl1' 'andAlso'@

@satisfyAll = 'foldr1' 'andAlso'@

@satisfyAll = 'foldl' 'andAlso' 'mempty'@

@satisfyAll = 'foldr' 'andAlso' 'mempty'@
-}
satisfyAll :: Foldable t => t (Validator e inp a) -> Validator e inp a
satisfyAll = foldr1 andAlso
{-# INLINABLE satisfyAll #-}
{-# SPECIALIZE satisfyAll :: [Validator e inp a] -> Validator e inp a #-}

---------------------------------------------------------------------
-- Type specific 'Validator's
---------------------------------------------------------------------

{- | Build a validator that runs given validator only if input is 'Just'.

Yields 'Success' when input is 'Nothing'.

__Note__: This sets the output of the new validator to /unit/.

==== __Examples__

>>> runValidator (optionally (failureIf even "Even")) (Just 5)
Success (Just 5)
>>> runValidator (optionally (failureIf even "Even"))) (Just 6)
Failure ("Even" :| [])
>>> runValidator (optionally (failureIf even "Even")) Nothing
Success Nothing
-}
optionally :: Validator e a x -> Validator e (Maybe a) (Maybe a)
optionally (Validator v) = Validator $ \x -> maybe (Success x) (fmap (const x) . v) x

-- | Utility to convert a regular predicate function to a 'Validator'. __INTERNAL__
validatorFrom :: (a -> Bool) -> e -> Validator e a a
validatorFrom predc err = Validator $ \x -> if predc x then Success x else Failure err
