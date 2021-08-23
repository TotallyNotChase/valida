module Main
    ( main
    ) where

import Data.Either        (isRight)
import Data.Foldable      (Foldable (fold))
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe         (isNothing)
import Data.Monoid        (Sum)

import           Test.Tasty            (TestTree, defaultMain, testGroup)
import           Test.Tasty.HUnit      (testCase, (@?=))
import qualified Test.Tasty.QuickCheck as QC
import qualified Test.Tasty.SmallCheck as SC

import Valida (Validation (..), Validator (runValidator), failureIf, failureIf', failureUnless, label,
               failureUnless', failures, failV, fromEither, negateV, negateV', partitionValidations,
               satisfyAll, satisfyAny, successes, toEither, fixV, validation, validationConst, (-?>), (</>),
               (<?>))

import Gen   (NonEmptyLQ, ValidationQ (..))
import Utils (neSingleton)
import Data.Functor

-- | Helper to build a validator with the raw given error.
validatorFrom :: (a -> Bool) -> e -> Validator e a a
validatorFrom predc err = failureUnless' predc <?> err

-- | Test the primitive non empty combinators.
testPrimCombs :: [TestTree]
testPrimCombs =
  [ testCase "failureIf fails with expected error when predicate yields true" $ do
      testValidator (failureIf $ (<7) . sum, "Sum is too small.", [1, 2] :: [Int], const . Failure . neSingleton)
      testValidator (failureIf $ (<7) . sum, "Sum is too small.", [5, 1] :: [Int], const . Failure . neSingleton)
      testValidator (failureIf (==0), "Equal to 0", 0 :: Int, const . Failure . neSingleton)
      testValidator (failureIf null, "Is empty", [] :: [()], const . Failure . neSingleton)
  , testCase "failureIf succeeds when predicate yields false" $ do
      testValidator (failureIf (elem 'c'), "Has the letter 'c'", "foo", const Success)
      testValidator (failureIf isNothing, "Is nothing", Just (), const Success)
      testValidator (failureIf isRight, "Is right (Either)", Left () :: Either () (), const Success)
      testValidator (failureIf or, "Has True", [False, False, False], const Success)
  , testCase "failureUnless succeeds when predicate yields true" $ do
      testValidator (failureUnless $ (<7) . sum, "Sum is too small.", [1, 2] :: [Int], const Success)
      testValidator (failureUnless $ (<7) . sum, "Sum is too small.", [5, 1] :: [Int], const Success)
      testValidator (failureUnless (==0), "Equal to 0", 0 :: Int, const Success)
      testValidator (failureUnless null, "Is empty", [] :: [()], const Success)
  , testCase "failureUnless fails with expected error when predicate yields false" $ do
      testValidator (failureUnless (elem 'c'), "Has the letter 'c'", "foo", const . Failure . neSingleton)
      testValidator (failureUnless isNothing, "Is nothing", Just (), const . Failure . neSingleton)
      testValidator (failureUnless isRight, "Is right (Either)", Left () :: Either () (), const . Failure . neSingleton)
      testValidator (failureUnless or, "Has True", [False, False, False], const . Failure . neSingleton)
  ]
  where
    testValidator ~(validator, err, inp, expct) = validator err `runValidator` inp @?= expct err inp

-- | Test the primitive unit combinators.
testPrimCombs' :: [TestTree]
testPrimCombs' =
  [ testCase "failureIf fails with expected error when predicate yields true" $ do
      testValidator (failureIf' ((<7) . sum), [1, 2] :: [Int], const $ Failure ())
      testValidator (failureIf' ((<7) . sum), [5, 1] :: [Int], const $ Failure ())
      testValidator (failureIf' (==0), 0 :: Int, const $ Failure ())
      testValidator (failureIf' null, [] :: [()], const $ Failure ())
  , testCase "failueIf succeeds when predicate yields false" $ do
      testValidator (failureIf' (elem 'c'), "foo", Success)
      testValidator (failureIf' isNothing, Just (), Success)
      testValidator (failureIf' isRight, Left () :: Either () (), Success)
      testValidator (failureIf' or, [False, False, False], Success)
  , testCase "failureUnless succeeds when predicate yields true" $ do
      testValidator (failureUnless' ((<7) . sum), [1, 2] :: [Int], Success)
      testValidator (failureUnless' ((<7) . sum), [5, 1] :: [Int], Success)
      testValidator (failureUnless' (==0), 0 :: Int, Success)
      testValidator (failureUnless' null, [] :: [()], Success)
  , testCase "failureUnless fails with expected error when predicate yields false" $ do
      testValidator (failureUnless' (elem 'c'), "foo", const $ Failure ())
      testValidator (failureUnless' isNothing, Just (), const $ Failure ())
      testValidator (failureUnless' isRight, Left () :: Either () (), const $ Failure ())
      testValidator (failureUnless' or, [False, False, False], const $ Failure ())
  ]
  where
    testValidator ~(validator, inp, expct) = validator `runValidator` inp @?= expct inp

-- | Test the primitive unit combinators with labeled errors.
testPrimCombs'' :: [TestTree]
testPrimCombs'' =
  [ testCase "failureIf fails with expected error when predicate yields true" $ do
      testValidator (failureIf' ((<7) . sum), "Sum is too small.", [1, 2] :: [Int], Failure)
      testValidator (failureIf' ((<7) . sum), "Sum is too small.", [5, 1] :: [Int], Failure)
      testValidator (failureIf' (==0), "Equal to 0", 0 :: Int, Failure)
      testValidator (failureIf' null, "Is empty", [] :: [()], Failure)
  , testCase "failureUnless fails with expected error when predicate yields false" $ do
      testValidator (failureUnless' (elem 'c'), "Has the letter 'c'", "foo", Failure)
      testValidator (failureUnless' isNothing, "Is nothing", Just (), Failure)
      testValidator (failureUnless' isRight, "Is right (Either)", Left () :: Either () (), Failure)
      testValidator (failureUnless' or, "Has True", [False, False, False], Failure)
  ]
  where
    testValidator ~(validator, err, inp, expct) = (validator <?> err) `runValidator` inp @?= expct err

-- | Test multiple validators combined.
testValidatorCollc :: [TestTree]
testValidatorCollc =
  [ testCase "Applicative validation fails if at least one validator fails" $ do
      testValidator ((\x _ _ -> x) <$> validatorOf even <*> validatorOf (>0) <*> validatorOf (<9)
        , 5 :: Int
        , const $ Failure ()
        )
      testValidator (const <$> validatorOf ((<7) . sum) <*> validatorOf ((>9) . length)
        , [1, 3, 7, 6] :: [Int]
        , const $ Failure ()
        )
      testValidator ((,) <$> fst -?> failureUnless' (>7) <*> snd -?> failureUnless' (<9)
        , (7, 7) :: (Int, Int)
        , const $ Failure ()
        )
  , testCase "Applicative validation succeeds if all validators suceed" $ do
      testValidator ((\x _ _ -> x) <$> validatorOf even <*> validatorOf (>0) <*> validatorOf (<9)
        , 6 :: Int
        , Success
        )
      testValidator (const <$> validatorOf ((<7) . sum) <*> validatorOf ((>9) . length)
        , [1, 0, 0, 0, 0, 0, 0, 3, 2, 0] :: [Int]
        , Success
        )
      testValidator ((,) <$> fst -?> failureUnless' (>7) <*> snd -?> failureUnless' (<5)
        , (8, 3) :: (Int, Int)
        , Success
        )
  ]
  where
    validatorOf f = fixV $ validatorFrom f ()
    testValidator ~(validator, inp, expct) = runValidator validator inp @?= expct inp

-- | Test utility functions for Validation.
testValidationUtils :: [TestTree]
testValidationUtils =
  [ QC.testProperty "(QC) fromEither . toEither = id"
      (vIdentityTest :: ValidationQ String Int -> Bool)
  , SC.testProperty "(SC) fromEither . toEither = id"
      (vIdentityTest :: ValidationQ Char Char -> Bool)
  , QC.testProperty "(QC) toEither . fromEither = id"
      (eIdentityTest :: Either Int Char -> Bool)
  , SC.testProperty "(SC) toEither . fromEither = id"
      (eIdentityTest :: Either (Char, Char) Bool -> Bool)
  , QC.testProperty "(QC) either f g (toEither v) = validation f g v"
      (vCatamorphTest :: ValidationQ String Int -> (String -> Char) -> (Int -> Char) -> Bool)
  , QC.testProperty "(QC) validation f g (fromEither v) = either f g v"
      (eCatamorphTest :: Either String Int -> (String -> Char) -> (Int -> Char) -> Bool)
  , QC.testProperty "(QC) either (const f) (const g) (toEither v) == validationConst f g v"
      (vCatamorphCnstTest :: ValidationQ String Int -> Maybe Float -> Maybe Float -> Bool)
  , SC.testProperty "(SC) either (const f) (const g) (toEither v) == validationConst f g v"
      (vCatamorphCnstTest :: ValidationQ Bool Char -> String -> String -> Bool)
  , QC.testProperty "(QC) validationConst f g (fromEither e) == either (const f) (const g) e"
      (eCatamorphCnstTest :: Either String Int -> Maybe Float -> Maybe Float -> Bool)
  , SC.testProperty "(SC) validationConst f g (fromEither e) == either (const f) (const g) e"
      (eCatamorphCnstTest :: Either Bool Char -> String -> String -> Bool)
  , QC.testProperty "(QC) validationConst f g v == validation (const f) (const g) v"
      (vvCatamorphCnstTest :: ValidationQ String Int -> Maybe Float -> Maybe Float -> Bool)
  , SC.testProperty "(SC) validationConst f g v == validation (const f) (const g) v"
      (vvCatamorphCnstTest :: ValidationQ Bool Char -> String -> String -> Bool)
  , QC.testProperty "(QC) partitionValidations xs = (failures xs, successes xs)"
      (partitionTest :: [ValidationQ Int Char] -> Bool)
  , SC.testProperty "(SC) partitionValidations xs = (failures xs, successes xs)"
      (partitionTest :: [ValidationQ Bool (Maybe Char)] -> Bool)
  ]
  where
    vIdentityTest :: (Eq e, Eq a) => ValidationQ e a -> Bool
    vIdentityTest (ValidationQ v) = fromEither (toEither v) == v
    eIdentityTest :: (Eq e, Eq a) => Either e a -> Bool
    eIdentityTest e = toEither (fromEither e) == e
    vCatamorphTest :: Eq c => ValidationQ e a -> (e -> c) -> (a -> c) -> Bool
    vCatamorphTest (ValidationQ v) f g = either f g (toEither v) == validation f g v
    eCatamorphTest :: Eq c => Either e a -> (e -> c) -> (a -> c) -> Bool
    eCatamorphTest e f g = validation f g (fromEither e) == either f g e
    vCatamorphCnstTest :: Eq c => ValidationQ e a -> c -> c -> Bool
    vCatamorphCnstTest (ValidationQ v) f g = either (const f) (const g) (toEither v) == validationConst f g v
    eCatamorphCnstTest :: Eq c => Either e a -> c -> c -> Bool
    eCatamorphCnstTest e f g = validationConst f g (fromEither e) == either (const f) (const g) e
    vvCatamorphCnstTest :: Eq c => ValidationQ e a -> c -> c -> Bool
    vvCatamorphCnstTest (ValidationQ v) f g = validationConst f g v == validation (const f) (const g) v
    partitionTest :: (Eq e, Eq a) => [ValidationQ e a] -> Bool
    partitionTest vqs = let xs = [v | ValidationQ v <- vqs]
        in partitionValidations xs == (failures xs, successes xs)

-- | Test the label and label functions.
testLabel :: [TestTree]
testLabel =
  [ QC.testProperty "(QC) Validator should yield relabeled Bool error upon failure"
      (labelTest :: Sum Int -> Maybe Char -> String -> Bool)
  , SC.testProperty "(SC) Validator should yield relabeled Bool error upon failure"
      (labelTest :: [Int] -> Bool -> Char -> Bool)
  ]
  where
    labelTest :: (Eq a, Eq e) => a -> e1 -> e -> Bool
    labelTest inp err err' = runValidator (fixV (failureUnless (const False) err) <?> err') inp == Failure err'

-- | Test validation errors being combined.
testErrorPreservation :: [TestTree]
testErrorPreservation =
  [ SC.testProperty "(SC) All errors from all validators should be combined upon failure"
      (helper :: (Bool, ()) -> NonEmpty [Bool] -> Bool)
  ]
  where
    helper :: (Traversable t, Monoid e, Eq e, Eq (t inp)) => inp -> t e -> Bool
    helper inp errs = runValidator
        (traverse (\e -> fixV $ label e $ failureUnless' (const False)) errs)
        inp == Failure (fold errs)

-- | Test the relation between NonEmpty and Unit combinators.
testNEUnitRelation :: [TestTree]
testNEUnitRelation =
  [ QC.testProperty "(QC) failureIf p err = label (const (neSingleton err)) (failureIf' p)"
      ((\predc inp err -> QC.classify (predc inp) "Significant"
        $ QC.classify (not $ predc inp) "Trivial"
        $ failureIf predc err `runValidator` inp
        == (failureIf' predc <?> neSingleton err) `runValidator` inp
       ) :: (Char -> Bool) -> Char -> String -> QC.Property
      )
  , SC.testProperty "(SC) failureIf p err = label (const (neSingleton err)) (failureIf' p)"
      ((\predc inp err -> failureIf predc err `runValidator` inp
        == (failureIf' predc <?> neSingleton err) `runValidator` inp
       ) :: (Bool -> Bool) -> Bool -> Int -> Bool
      )
  , QC.testProperty "(QC) failureUnless p err = label (const (neSingleton err)) (failureUnless' p)"
      ((\predc inp err -> QC.classify (not $ predc inp) "Significant"
        $ QC.classify (predc inp) "Trivial"
        $ failureUnless predc err `runValidator` inp
        == (failureUnless' predc <?> neSingleton err) `runValidator` inp
       ) :: (Char -> Bool) -> Char -> String -> QC.Property
      )
  , SC.testProperty "(SC) failureUnless p err = label (const (neSingleton err)) (failureUnless' p)"
      ((\predc inp err -> failureUnless predc err `runValidator` inp
        == (failureUnless' predc <?> neSingleton err) `runValidator` inp
       ) :: (Bool -> Bool) -> Bool -> Int -> Bool
      )
  ]

-- | Test the negateV function.
testnegateV :: [TestTree]
testnegateV =
  [ QC.testProperty "(QC) negateV . negateV = id (assuming errors are unchanged)"
      (helper :: (String -> Bool, Int) -> String -> Bool)
  , SC.testProperty "(SC) negateV . negateV = id (assuming errors are unchanged)"
      (helper :: (Bool -> Bool, [Bool]) -> Bool -> Bool)
  ]
  where
    helper :: (Eq a, Eq e) => (a -> Bool, e) -> a -> Bool
    helper ~(predc, err) x = let validator = validatorFrom predc err
        in negateV err (negateV err validator) `runValidator` x == validator `runValidator` x
        && negateV' (negateV' validator) `runValidator` x == (validator <?> ()) `runValidator` x

-- | Test the relation between failureIf and failureUnless.
testIfUnlessRelation :: [TestTree]
testIfUnlessRelation =
  [ QC.testProperty "(QC) failureIf' p = failureUnless' (not . p)"
      (helper :: (String -> Bool) -> String -> Bool)
  , SC.testProperty "(SC) failureIf' p = failureUnless' (not . p)"
      (helper :: (Bool -> Bool) -> Bool -> Bool)
  , QC.testProperty "(QC) failureIf p err = negateV (neSingleton err) (failureUnless' p)"
      (negateHelper :: (String -> Bool, String) -> String -> Bool)
  , SC.testProperty "(SC) failureIf p err = negateV (neSingleton err) (failureUnless' p)"
      (negateHelper :: (Bool -> Bool, Char) -> Bool -> Bool)
  , QC.testProperty "(QC) failureIf' p = negateV' (failureUnless' p)"
      (negateHelper' :: (String -> Bool) -> String -> Bool)
  , SC.testProperty "(SC) failureIf' p = negateV' (failureUnless' p)"
      (negateHelper' :: (Bool -> Bool) -> Bool -> Bool)
  ]
  where
    helper :: Eq a => (a -> Bool) -> a -> Bool
    helper predc inp = failureIf' predc `runValidator` inp
        == failureUnless' (not . predc) `runValidator` inp
    negateHelper :: (Eq a, Eq e) => (a -> Bool, e) -> a -> Bool
    negateHelper ~(predc, err) inp = failureIf predc err `runValidator` inp
        == negateV (neSingleton err) (failureUnless' predc) `runValidator` inp
        && failureUnless predc err `runValidator` inp
        == negateV (neSingleton err) (failureIf' predc) `runValidator` inp
    negateHelper' :: Eq a => (a -> Bool) -> a -> Bool
    negateHelper' predc inp = failureIf' predc `runValidator` inp
        == negateV' (failureUnless' predc) `runValidator` inp
        && failureUnless' predc `runValidator` inp
        == negateV' (failureIf' predc) `runValidator` inp

-- | Test orElse properties.
testOrElse :: [TestTree]
testOrElse =
  [ QC.testProperty "(QC) failV always fails"
      (failVTest :: String -> Bool)
  , SC.testProperty "(SC) failV always fails"
      (failVTest :: Maybe Char -> Bool)
  , QC.testProperty "(QC) Associativity: validator1 </> (validator2 </> validator3) = (validator1 </> validator2) </> validator3"
      (asscTest :: (Char -> Bool, String) -> (Char -> Bool, String) -> (Char -> Bool, String) -> Char -> Bool)
  , SC.testProperty "(SC) Associativity: validator1 </> (validator2 </> validator3) = (validator1 </> validator2) </> validator3"
      (asscTest :: (Bool -> Bool, [()]) -> (Bool -> Bool, [()]) -> (Bool -> Bool, [()]) -> Bool -> Bool)
  , QC.testProperty "(QC) Identity: validator </> failV = failV </> validator = validator"
      (identTest :: (Int -> Bool, String) -> Int -> Bool)
  , SC.testProperty "(SC) Identity: validator </> failV = failV </> validator = validator"
      (identTest :: (Bool -> Bool, [Bool]) -> Bool -> Bool)
  , QC.testProperty "(QC) Annihilator: validator </> mempty = mempty </> validator = mempty"
      (annihilatorTest :: ([Int] -> Bool, String) -> [Int] -> Bool)
  , SC.testProperty "(SC) Annihilator: validator </> mempty = mempty </> validator = mempty"
      (annihilatorTest :: (Maybe Bool -> Bool, [Bool]) -> Maybe Bool -> Bool)
  , QC.testProperty "(QC) Complement: validator </> (negateV e validator) = (negateV e validator) </> validator = Success"
      (complmTest :: (Int -> Bool, String, String) -> Int -> Bool)
  , SC.testProperty "(SC) Complement: validator </> (negateV e validator) = (negateV e validator) </> validator = Success"
      (complmTest :: (Bool -> Bool, [Bool], [Bool]) -> Bool -> Bool)
  ]
  where
    failVTest :: Eq a => a -> Bool
    failVTest = (==Failure (mempty :: String)) . runValidator (fixV failV)
    asscTest :: (Eq a, Eq e, Semigroup e) => (a -> Bool, e) -> (a -> Bool, e) -> (a -> Bool, e) -> a -> Bool
    asscTest ~(f, err1) ~(g, err2) ~(h, err3) x =
        let ~(validator1, validator2, validator3) = (validatorFrom f err1, validatorFrom g err2, validatorFrom h err3)
        in  (validator1 </> (validator2 </> validator3)) `runValidator` x == ((validator1 </> validator2) </> validator3) `runValidator` x
    identTest :: (Eq a, Eq e, Monoid e) => (a -> Bool, e) -> a -> Bool
    identTest ~(f, err) x = let validator = validatorFrom f err
        in (validator </> failV) `runValidator` x == validator `runValidator` x
        && (failV </> validator) `runValidator` x == validator `runValidator` x
    annihilatorTest :: (Eq e, Monoid e) => (a -> Bool, e) -> a -> Bool
    annihilatorTest ~(f, err) x = let validator = void $ validatorFrom f err
        in (validator </> mempty) `runValidator` x == Success ()
        && (mempty </> validator) `runValidator` x == Success ()
    complmTest :: (Eq a, Eq e, Semigroup e) => (a -> Bool, e, e) -> a -> Bool
    complmTest ~(f, err, err') x =
        let validator = validatorFrom f err
        in let complmvalidator = negateV err' validator
        in (validator </> complmvalidator) `runValidator` x == Success x
        && (complmvalidator </> validator) `runValidator` x == Success x

{-# ANN testAndAlso "HLint: ignore Monoid law, right identity" #-}
{-# ANN testAndAlso "HLint: ignore Monoid law, left identity" #-}
-- | Test andAlso properties.
testAndAlso :: [TestTree]
testAndAlso =
  [ QC.testProperty "(QC) mempty always succeeds"
      (memptyTest :: String -> Bool)
  , SC.testProperty "(SC) mempty always succeeds"
      (memptyTest :: Maybe Char -> Bool)
  , QC.testProperty "(QC) Associativity: validator1 <> (validator2 <> validator3) = (validator1 <> validator2) <> validator3"
      (asscTest :: (Char -> Bool, String) -> (Char -> Bool, String) -> (Char -> Bool, String) -> Char -> Bool)
  , SC.testProperty "(SC) Associativity: validator1 <> (validator2 <> validator3) = (validator1 <> validator2) <> validator3"
      (asscTest :: (Bool -> Bool, [()]) -> (Bool -> Bool, [()]) -> (Bool -> Bool, [()]) -> Bool -> Bool)
  , QC.testProperty "(QC) Identity: validator <> mempty = mempty <> validator = validator"
      (identTest :: ([Int] -> Bool, String) -> [Int] -> Bool)
  , SC.testProperty "(SC) Identity: validator <> mempty = mempty <> validator = validator"
      (identTest :: (Maybe Bool -> Bool, [Bool]) -> Maybe Bool -> Bool)
  , QC.testProperty "(QC) Annihilator: validator <> failV = failV <> validator = failV"
      (annihilatorTest :: (Int -> Bool, String) -> Int -> Bool)
  , SC.testProperty "(SC) Annihilator: validator <> failV = failV <> validator = failV"
      (annihilatorTest :: (Bool -> Bool, [Bool]) -> Bool -> Bool)
  , QC.testProperty "(QC) Complement: validator <> (negateV e validator) = (negateV e validator) <> validator = Failure"
      (complmTest :: (Int -> Bool, String, String) -> Int -> Bool)
  , SC.testProperty "(SC) Complement: validator <> (negateV e validator) = (negateV e validator) <> validator = Failure"
      (complmTest :: (Bool -> Bool, [Bool], [Bool]) -> Bool -> Bool)
  , QC.testProperty "(QC) Idempotence: validator <> validator = validator"
      (idempTest :: (Int -> Bool, String) -> Int -> Bool)
  , SC.testProperty "(SC) Idempotence: validator <> validator = validator"
      (idempTest :: (Bool -> Bool, [Bool]) -> Bool -> Bool)
  ]
  where
    memptyTest :: a -> Bool
    memptyTest x = (Success () `asTypeOf` Failure "")
        == runValidator mempty x
    asscTest :: (Eq a, Eq e) => (a -> Bool, e) -> (a -> Bool, e) -> (a -> Bool, e) -> a -> Bool
    asscTest ~(f, err1) (g, err2) (h, err3) x =
        let ~(validator1, validator2, validator3) = (validatorFrom f err1, validatorFrom g err2, validatorFrom h err3)
        in (validator1 <> (validator2 <> validator3)) `runValidator` x == ((validator1 <> validator2) <> validator3) `runValidator` x
    identTest :: Eq e => (a -> Bool, e) -> a -> Bool
    identTest ~(f, err) x = let validator = void $ validatorFrom f err
        in (validator <> mempty) `runValidator` x == validator `runValidator` x
        && (mempty <> validator) `runValidator` x == validator `runValidator` x
    annihilatorTest :: (Eq a, Eq e, Monoid e) => (a -> Bool, e) -> a -> Bool
    annihilatorTest ~(f, err) x = let validator = validatorFrom f err
        in (validator <> failV) `runValidator` x == Failure (if f x then mempty else err)
        && (failV <> validator) `runValidator` x == Failure mempty
    complmTest :: (Eq a, Eq e) => (a -> Bool, e, e) -> a -> Bool
    complmTest ~(f, err, err') x =
        let validator = validatorFrom f err
        in let complmvalidator = negateV err' validator
        in (validator <> complmvalidator) `runValidator` x == Failure (if f x then err' else err)
        && (complmvalidator <> validator) `runValidator` x == Failure (if f x then err' else err)
    idempTest :: (Eq a, Eq e) => (a -> Bool, e) -> a -> Bool
    idempTest ~(f, err) x = let validator = validatorFrom f err
        in (validator <> validator) `runValidator` x == validator `runValidator` x

-- | Test the satisfyAny function.
testSatisfyAny :: [TestTree]
testSatisfyAny =
  [ QC.testProperty "satisfyAny = foldl1 orElse"
      (leftFold1Test :: NonEmptyLQ (Int -> Bool, Sum Int) -> Int -> Bool)
  , SC.testProperty "satisfyAny = foldl1 orElse"
      (leftFold1Test :: NonEmptyLQ (Bool -> Bool, Maybe [Bool]) -> Bool -> Bool)
  , QC.testProperty "satisfyAny = foldr1 orElse"
      (rightFold1Test :: NonEmptyLQ (Int -> Bool, Sum Int) -> Int -> Bool)
  , SC.testProperty "satisfyAny = foldr1 orElse"
      (rightFold1Test :: NonEmptyLQ (Bool -> Bool, Maybe [Bool]) -> Bool -> Bool)
  , QC.testProperty "satisfyAny = foldl orElse failV"
      (leftFoldTest :: NonEmptyLQ (Int -> Bool, Sum Int) -> Int -> Bool)
  , SC.testProperty "satisfyAny = foldl orElse failV"
      (leftFoldTest :: NonEmptyLQ (Bool -> Bool, Maybe [Bool]) -> Bool -> Bool)
  , QC.testProperty "satisfyAny = foldr orElse failV"
      (rightFoldTest :: NonEmptyLQ (Int -> Bool, Sum Int) -> Int -> Bool)
  , SC.testProperty "satisfyAny = foldr orElse failV"
      (rightFoldTest :: NonEmptyLQ (Bool -> Bool, Maybe [Bool]) -> Bool -> Bool)
  ]
  where
    leftFold1Test :: (Eq a, Eq e, Semigroup e) => NonEmptyLQ (a -> Bool, e) -> a -> Bool
    leftFold1Test predcs x = let validators = uncurry validatorFrom <$> predcs
        in satisfyAny validators `runValidator` x
        == foldl1 (</>) validators `runValidator` x
    rightFold1Test :: (Eq a, Eq e, Semigroup e) => NonEmptyLQ (a -> Bool, e) -> a -> Bool
    rightFold1Test predcs x = let validators = uncurry validatorFrom <$> predcs
        in satisfyAny validators `runValidator` x
        == foldr1 (</>) validators `runValidator` x
    leftFoldTest :: (Eq a, Eq e, Monoid e) => NonEmptyLQ (a -> Bool, e) -> a -> Bool
    leftFoldTest predcs x = let validators = uncurry validatorFrom <$> predcs
        in satisfyAny validators `runValidator` x
        == foldl (</>) failV validators `runValidator` x
    rightFoldTest :: (Eq a, Eq e, Monoid e) => NonEmptyLQ (a -> Bool, e) -> a -> Bool
    rightFoldTest predcs x = let validators = uncurry validatorFrom <$> predcs
        in satisfyAny validators `runValidator` x
        == foldr (</>) failV validators `runValidator` x

{-# ANN testSatisfyAll "HLint: ignore Use mconcat" #-}
-- | Test the satisfyAll function.
testSatisfyAll :: [TestTree]
testSatisfyAll =
  [ QC.testProperty "satisfyAll = fold"
      (foldTest :: NonEmptyLQ (Int -> Bool, Sum Int) -> Int -> Bool)
  , SC.testProperty "satisfyAll = fold"
      (foldTest :: NonEmptyLQ (Bool -> Bool, Maybe [Bool]) -> Bool -> Bool)
  , QC.testProperty "satisfyAll = foldl1 andAlso"
      (leftFold1Test :: NonEmptyLQ (Int -> Bool, Sum Int) -> Int -> Bool)
  , SC.testProperty "satisfyAll = foldl1 andAlso"
      (leftFold1Test :: NonEmptyLQ (Bool -> Bool, Maybe [Bool]) -> Bool -> Bool)
  , QC.testProperty "satisfyAll = foldr1 andAlso"
      (rightFold1Test :: NonEmptyLQ (Int -> Bool, Sum Int) -> Int -> Bool)
  , SC.testProperty "satisfyAll = foldr1 andAlso"
      (rightFold1Test :: NonEmptyLQ (Bool -> Bool, Maybe [Bool]) -> Bool -> Bool)
  , QC.testProperty "satisfyAll = foldl andAlso mempty"
      (leftFoldTest :: NonEmptyLQ (Int -> Bool, Sum Int) -> Int -> Bool)
  , SC.testProperty "satisfyAll = foldl andAlso mempty"
      (leftFoldTest :: NonEmptyLQ (Bool -> Bool, Maybe [Bool]) -> Bool -> Bool)
  , QC.testProperty "satisfyAll = foldr andAlso mempty"
      (rightFoldTest :: NonEmptyLQ (Int -> Bool, Sum Int) -> Int -> Bool)
  , SC.testProperty "satisfyAll = foldr andAlso mempty"
      (rightFoldTest :: NonEmptyLQ (Bool -> Bool, Maybe [Bool]) -> Bool -> Bool)
  ]
  where
    foldTest :: Eq e => NonEmptyLQ (a -> Bool, e) -> a -> Bool
    foldTest predcs x = let validators = fmap void $ uncurry validatorFrom <$> predcs
        in satisfyAll validators `runValidator` x
        == fold validators `runValidator` x
    leftFold1Test :: (Eq a, Eq e) => NonEmptyLQ (a -> Bool, e) -> a -> Bool
    leftFold1Test predcs x = let validators = uncurry validatorFrom <$> predcs
        in satisfyAll validators `runValidator` x
        == foldl1 (<>) validators `runValidator` x
    rightFold1Test :: (Eq a, Eq e) => NonEmptyLQ (a -> Bool, e) -> a -> Bool
    rightFold1Test predcs x = let validators = uncurry validatorFrom <$> predcs
        in satisfyAll validators `runValidator` x
        == foldr1 (<>) validators `runValidator` x
    leftFoldTest :: Eq e => NonEmptyLQ (a -> Bool, e) -> a -> Bool
    leftFoldTest predcs x = let validators = fmap void $ uncurry validatorFrom <$> predcs
        in satisfyAll validators `runValidator` x
        == foldl (<>) mempty validators `runValidator` x
    rightFoldTest :: Eq e => NonEmptyLQ (a -> Bool, e) -> a -> Bool
    rightFoldTest predcs x = let validators = fmap void $ uncurry validatorFrom <$> predcs
        in satisfyAll validators `runValidator` x
        == foldr (<>) mempty validators `runValidator` x

-- | Test Validator combinators.
testCombs :: [TestTree]
testCombs =
  [ testGroup "Test primitive NonEmpty combinators" testPrimCombs
  , testGroup "Test primitive Unit combinators" testPrimCombs'
  , testGroup "Test primitive Unit combinators with labeled error" testPrimCombs''
  , testGroup "Test relationship between primitive NonEmpty and Unit combinators" testNEUnitRelation
  , testGroup "Test relationship between primitive 'if' and 'unless' combinators" testIfUnlessRelation
  ]

-- | Test functions that combine Validators.
testCombMix :: [TestTree]
testCombMix =
  [ testGroup "Test `orElse` function" testOrElse
  , testGroup "Test `andAlso` function" testAndAlso
  , testGroup "Test `satisfyAny` function" testSatisfyAny
  , testGroup "Test `satisfyAll` function" testSatisfyAll
  ]

main :: IO ()
main = defaultMain $ testGroup "Test suite"
  [ testGroup "Test Validator combinators" testCombs
  , testGroup "Test negateV function" testnegateV
  , testGroup "Test Validator combining functions" testCombMix
  , testGroup "Test validation of a collection of Validators" testValidatorCollc
  , testGroup "Test Validation utilities" testValidationUtils
  , testGroup "Test Validator relabeling" testLabel
  , testGroup "Test Validator error grouping" testErrorPreservation
  ]
