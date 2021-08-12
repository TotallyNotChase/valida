module Main
    ( main
    ) where

import Data.Bool     (bool)
import Data.Either   (isRight)
import Data.Foldable (Foldable (fold))
import Data.Maybe    (isNothing)
import Data.Monoid   (Sum)

import           Test.Tasty            (TestTree, defaultMain, testGroup)
import           Test.Tasty.HUnit      (testCase, (@?=))
import qualified Test.Tasty.QuickCheck as QC
import qualified Test.Tasty.SmallCheck as SC

import Valida (Validation (..), ValidationRule, Validator (validate), failureIf, failureIf', failureUnless,
               failureUnless', falseRule, fromEither, negateRule, negateRule', satisfyAll, satisfyAny, toEither,
               validation, verify, vrule, (-?>), (</>), (<?>))

import Gen   (NonEmptyLQ, ValidationQ (..))
import Utils (singleton)

predToVRule :: e -> (a -> Bool) -> ValidationRule e a
predToVRule err f = vrule $ bool (Failure err) (Success ()) . f

-- | Test the primitive non empty combinators.
testPrimCombs :: [TestTree]
testPrimCombs =
  [ testCase "failureIf fails with expected error when predicate yields true" $ do
      testValidator (failureIf $ (<7) . sum, "Sum is too small.", [1, 2] :: [Int], const . Failure . singleton)
      testValidator (failureIf $ (<7) . sum, "Sum is too small.", [5, 1] :: [Int], const . Failure . singleton)
      testValidator (failureIf (==0), "Equal to 0", 0 :: Int, const . Failure . singleton)
      testValidator (failureIf null, "Is empty", [] :: [()], const . Failure . singleton)
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
      testValidator (failureUnless (elem 'c'), "Has the letter 'c'", "foo", const . Failure . singleton)
      testValidator (failureUnless isNothing, "Is nothing", Just (), const . Failure . singleton)
      testValidator (failureUnless isRight, "Is right (Either)", Left () :: Either () (), const . Failure . singleton)
      testValidator (failureUnless or, "Has True", [False, False, False], const . Failure . singleton)
  ]
  where
    testValidator (rule, err, inp, expct) = validate (verify $ rule err) inp @?= expct err inp

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
    testValidator (rule, inp, expct) = validate (verify rule) inp @?= expct inp

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
    testValidator (rule, err, inp, expct) = validate (verify $ rule <?> const err) inp @?= expct err

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
    validatorOf f = verify $ predToVRule () f
    testValidator (validator, inp, expct) = validate validator inp @?= expct inp

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

-- | Test the relation between NonEmpty and Unit combinators.
testNEUnitRelation :: [TestTree]
testNEUnitRelation =
  [ QC.testProperty "(QC) failureIf p err = label (const (singleton err)) (failureIf' p)"
      ((\predc inp err -> QC.classify (predc inp) "Significant"
        $ QC.classify (not $ predc inp) "Trivial"
        $ validate (verify $ failureIf predc err) inp
        == validate (verify $ failureIf' predc <?> const (singleton err)) inp
       ) :: (Char -> Bool) -> Char -> String -> QC.Property
      )
  , SC.testProperty "(SC) failureIf p err = label (const (singleton err)) (failureIf' p)"
      ((\predc inp err -> validate (verify $ failureIf predc err) inp
        == validate (verify $ failureIf' predc <?> const (singleton err)) inp
       ) :: (Bool -> Bool) -> Bool -> Int -> Bool
      )
  , QC.testProperty "(QC) failureUnless p err = label (const (singleton err)) (failureUnless' p)"
      ((\predc inp err -> QC.classify (not $ predc inp) "Significant"
        $ QC.classify (predc inp) "Trivial"
        $ validate (verify $ failureUnless predc err) inp
        == validate (verify $ failureUnless' predc <?> const (singleton err)) inp
       ) :: (Char -> Bool) -> Char -> String -> QC.Property
      )
  , SC.testProperty "(SC) failureUnless p err = label (const (singleton err)) (failureUnless' p)"
      ((\predc inp err -> validate (verify $ failureUnless predc err) inp
        == validate (verify $ failureUnless' predc <?> const (singleton err)) inp
       ) :: (Bool -> Bool) -> Bool -> Int -> Bool
      )
  ]

-- | Test the negateRule function.
testNegateRule :: [TestTree]
testNegateRule =
  [ QC.testProperty "(QC) negateRule . negateRule = id (assuming errors are unchanged)"
      (helper :: (String -> Bool, Int) -> String -> Bool)
  , SC.testProperty "(SC) negateRule . negateRule = id (assuming errors are unchanged)"
      (helper :: (Bool -> Bool, [Bool]) -> Bool -> Bool)
  ]
  where
    helper :: (Eq a, Eq e) => (a -> Bool, e) -> a -> Bool
    helper (predc, err) x = let rule = predToVRule err predc
        in validate (verify $ negateRule err . negateRule err $ rule) x == validate (verify rule) x
        && validate (verify $ negateRule' . negateRule' $ rule) x == validate (verify $ rule <?> const ()) x

-- | Test the relation between failureIf and failureUnless.
testIfUnlessRelation :: [TestTree]
testIfUnlessRelation =
  [ QC.testProperty "(QC) failureIf' p = failureUnless' (not . p)"
      (helper :: (String -> Bool) -> String -> Bool)
  , SC.testProperty "(SC) failureIf' p = failureUnless' (not . p)"
      (helper :: (Bool -> Bool) -> Bool -> Bool)
  , QC.testProperty "(QC) failureIf p err = negateRule (singleton err) (failureUnless' p)"
      (negateHelper :: (String -> Bool, String) -> String -> Bool)
  , SC.testProperty "(SC) failureIf p err = negateRule (singleton err) (failureUnless' p)"
      (negateHelper :: (Bool -> Bool, Char) -> Bool -> Bool)
  , QC.testProperty "(QC) failureIf' p = negateRule' (failureUnless' p)"
      (negateHelper' :: (String -> Bool) -> String -> Bool)
  , SC.testProperty "(SC) failureIf' p = negateRule' (failureUnless' p)"
      (negateHelper' :: (Bool -> Bool) -> Bool -> Bool)
  ]
  where
    helper :: Eq a => (a -> Bool) -> a -> Bool
    helper predc inp = validate (verify $ failureIf' predc) inp
        == validate (verify $ failureUnless' (not . predc)) inp
    negateHelper :: (Eq a, Eq e) => (a -> Bool, e) -> a -> Bool
    negateHelper (predc, err) inp = validate (verify $ failureIf predc err) inp
        == validate (verify $ negateRule (singleton err) $ failureUnless' predc) inp
        && validate (verify $ failureUnless predc err) inp
        == validate (verify $ negateRule (singleton err) $ failureIf' predc) inp
    negateHelper' :: Eq a => (a -> Bool) -> a -> Bool
    negateHelper' predc inp = validate (verify $ failureIf' predc) inp
        == validate (verify $ negateRule' $ failureUnless' predc) inp
        && validate (verify $ failureUnless' predc) inp
        == validate (verify $ negateRule' $ failureIf' predc) inp

-- | Test orElse properties.
testOrElse :: [TestTree]
testOrElse =
  [ QC.testProperty "(QC) falseRule always fails"
      (falseRuleTest :: String -> Bool)
  , SC.testProperty "(SC) falseRule always fails"
      (falseRuleTest :: Maybe Char -> Bool)
  , QC.testProperty "(QC) Associativity: rule1 </> (rule2 </> rule3) = (rule1 </> rule2) </> rule3"
      (asscTest :: (Char -> Bool, String) -> (Char -> Bool, String) -> (Char -> Bool, String) -> Char -> Bool)
  , SC.testProperty "(SC) Associativity: rule1 </> (rule2 </> rule3) = (rule1 </> rule2) </> rule3"
      (asscTest :: (Bool -> Bool, [()]) -> (Bool -> Bool, [()]) -> (Bool -> Bool, [()]) -> Bool -> Bool)
  , QC.testProperty "(QC) Identity: rule </> falseRule = falseRule </> rule = rule"
      (identTest :: (Int -> Bool, String) -> Int -> Bool)
  , SC.testProperty "(SC) Identity: rule </> falseRule = falseRule </> rule = rule"
      (identTest :: (Bool -> Bool, [Bool]) -> Bool -> Bool)
  , QC.testProperty "(QC) Annihilator: rule </> mempty = mempty </> rule = mempty"
      (annihilatorTest :: (Int -> Bool, String) -> Int -> Bool)
  , SC.testProperty "(SC) Annihilator: rule </> mempty = mempty </> rule = mempty"
      (annihilatorTest :: (Bool -> Bool, [Bool]) -> Bool -> Bool)
  , QC.testProperty "(QC) Complement: rule </> (negateRule e rule) = (negateRule e rule) </> rule = Success"
      (complmTest :: (Int -> Bool, String, String) -> Int -> Bool)
  , SC.testProperty "(SC) Complement: rule </> (negateRule e rule) = (negateRule e rule) </> rule = Success"
      (complmTest :: (Bool -> Bool, [Bool], [Bool]) -> Bool -> Bool)
  ]
  where
    falseRuleTest :: Eq a => a -> Bool
    falseRuleTest = (==Failure (mempty :: String)) . validate (verify falseRule)
    asscTest :: (Eq a, Eq e, Semigroup e) => (a -> Bool, e) -> (a -> Bool, e) -> (a -> Bool, e) -> a -> Bool
    asscTest (f, err1) (g, err2) (h, err3) x =
        let (rule1, rule2, rule3) = (predToVRule err1 f, predToVRule err2 g, predToVRule err3 h)
        in validate (verify $ rule1 </> (rule2 </> rule3)) x == validate (verify $ (rule1 </> rule2) </> rule3) x
    identTest :: (Eq a, Eq e, Monoid e) => (a -> Bool, e) -> a -> Bool
    identTest (f, err) x = let rule = predToVRule err f
        in validate (verify $ rule </> falseRule) x == validate (verify rule) x
        && validate (verify $ falseRule </> rule) x == validate (verify rule) x
    annihilatorTest :: (Eq a, Eq e, Monoid e) => (a -> Bool, e) -> a -> Bool
    annihilatorTest (f, err) x = let rule = predToVRule err f
        in validate (verify $ rule </> mempty) x == Success x
        && validate (verify $ mempty </> rule) x == Success x
    complmTest :: (Eq a, Eq e, Semigroup e) => (a -> Bool, e, e) -> a -> Bool
    complmTest (f, err, err') x =
        let rule = predToVRule err f
        in let complmRule = negateRule err' rule
        in validate (verify $ rule </> complmRule) x == Success x
        && validate (verify $ complmRule </> rule) x == Success x

{-# ANN testAndAlso "HLint: ignore Monoid law, right identity" #-}
{-# ANN testAndAlso "HLint: ignore Monoid law, left identity" #-}
-- | Test andAlso properties.
testAndAlso :: [TestTree]
testAndAlso =
  [ QC.testProperty "(QC) mempty always succeeds"
      (memptyTest :: String -> Bool)
  , SC.testProperty "(SC) mempty always succeeds"
      (memptyTest :: Maybe Char -> Bool)
  , QC.testProperty "(QC) Associativity: rule1 <> (rule2 <> rule3) = (rule1 <> rule2) <> rule3"
      (asscTest :: (Char -> Bool, String) -> (Char -> Bool, String) -> (Char -> Bool, String) -> Char -> Bool)
  , SC.testProperty "(SC) Associativity: rule1 <> (rule2 <> rule3) = (rule1 <> rule2) <> rule3"
      (asscTest :: (Bool -> Bool, [()]) -> (Bool -> Bool, [()]) -> (Bool -> Bool, [()]) -> Bool -> Bool)
  , QC.testProperty "(QC) Identity: rule <> mempty = mempty <> rule = rule"
      (identTest :: (Int -> Bool, String) -> Int -> Bool)
  , SC.testProperty "(SC) Identity: rule <> mempty = mempty <> rule = rule"
      (identTest :: (Bool -> Bool, [Bool]) -> Bool -> Bool)
  , QC.testProperty "(QC) Annihilator: rule <> falseRule = falseRule <> rule = falseRule"
      (annihilatorTest :: (Int -> Bool, String) -> Int -> Bool)
  , SC.testProperty "(SC) Annihilator: rule <> falseRule = falseRule <> rule = falseRule"
      (annihilatorTest :: (Bool -> Bool, [Bool]) -> Bool -> Bool)
  , QC.testProperty "(QC) Complement: rule <> (negateRule e rule) = (negateRule e rule) <> rule = Failure"
      (complmTest :: (Int -> Bool, String, String) -> Int -> Bool)
  , SC.testProperty "(SC) Complement: rule <> (negateRule e rule) = (negateRule e rule) <> rule = Failure"
      (complmTest :: (Bool -> Bool, [Bool], [Bool]) -> Bool -> Bool)
  , QC.testProperty "(QC) Idempotence: rule <> rule = rule"
      (idempTest :: (Int -> Bool, String) -> Int -> Bool)
  , SC.testProperty "(SC) Idempotence: rule <> rule = rule"
      (idempTest :: (Bool -> Bool, [Bool]) -> Bool -> Bool)
  ]
  where
    memptyTest :: Eq a => a -> Bool
    memptyTest = (==) <$> Success `asTypeOf` const (Failure "") <*> validate (verify mempty)
    asscTest :: (Eq a, Eq e) => (a -> Bool, e) -> (a -> Bool, e) -> (a -> Bool, e) -> a -> Bool
    asscTest (f, err1) (g, err2) (h, err3) x =
        let (rule1, rule2, rule3) = (predToVRule err1 f, predToVRule err2 g, predToVRule err3 h)
        in validate (verify $ rule1 <> (rule2 <> rule3)) x == validate (verify $ (rule1 <> rule2) <> rule3) x
    identTest :: (Eq a, Eq e) => (a -> Bool, e) -> a -> Bool
    identTest (f, err) x = let rule = predToVRule err f
        in validate (verify $ rule <> mempty) x == validate (verify rule) x
        && validate (verify $ mempty <> rule) x == validate (verify rule) x
    annihilatorTest :: (Eq a, Eq e, Monoid e) => (a -> Bool, e) -> a -> Bool
    annihilatorTest (f, err) x = let rule = predToVRule err f
        in validate (verify $ rule <> falseRule) x == Failure (if f x then mempty else err)
        && validate (verify $ falseRule <> rule) x == Failure mempty
    complmTest :: (Eq a, Eq e) => (a -> Bool, e, e) -> a -> Bool
    complmTest (f, err, err') x =
        let rule = predToVRule err f
        in let complmRule = negateRule err' rule
        in validate (verify $ rule <> complmRule) x == Failure (if f x then err' else err)
        && validate (verify $ complmRule <> rule) x == Failure (if f x then err' else err)
    idempTest :: (Eq a, Eq e) => (a -> Bool, e) -> a -> Bool
    idempTest (f, err) x = let rule = predToVRule err f
        in validate (verify $ rule <> rule) x == validate (verify rule) x

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
  , QC.testProperty "satisfyAny = foldl orElse falseRule"
      (leftFoldTest :: NonEmptyLQ (Int -> Bool, Sum Int) -> Int -> Bool)
  , SC.testProperty "satisfyAny = foldl orElse falseRule"
      (leftFoldTest :: NonEmptyLQ (Bool -> Bool, Maybe [Bool]) -> Bool -> Bool)
  , QC.testProperty "satisfyAny = foldr orElse falseRule"
      (rightFoldTest :: NonEmptyLQ (Int -> Bool, Sum Int) -> Int -> Bool)
  , SC.testProperty "satisfyAny = foldr orElse falseRule"
      (rightFoldTest :: NonEmptyLQ (Bool -> Bool, Maybe [Bool]) -> Bool -> Bool)
  ]
  where
    leftFold1Test :: (Eq a, Eq e, Semigroup e) => NonEmptyLQ (a -> Bool, e) -> a -> Bool
    leftFold1Test predcs x = let rules = (\(f, err) -> predToVRule err f) <$> predcs
        in validate (verify $ satisfyAny rules) x
        == validate (verify $ foldl1 (</>) rules) x
    rightFold1Test :: (Eq a, Eq e, Semigroup e) => NonEmptyLQ (a -> Bool, e) -> a -> Bool
    rightFold1Test predcs x = let rules = (\(f, err) -> predToVRule err f) <$> predcs
        in validate (verify $ satisfyAny rules) x
        == validate (verify $ foldr1 (</>) rules) x
    leftFoldTest :: (Eq a, Eq e, Monoid e) => NonEmptyLQ (a -> Bool, e) -> a -> Bool
    leftFoldTest predcs x = let rules = (\(f, err) -> predToVRule err f) <$> predcs
        in validate (verify $ satisfyAny rules) x
        == validate (verify $ foldl (</>) falseRule rules) x
    rightFoldTest :: (Eq a, Eq e, Monoid e) => NonEmptyLQ (a -> Bool, e) -> a -> Bool
    rightFoldTest predcs x = let rules = (\(f, err) -> predToVRule err f) <$> predcs
        in validate (verify $ satisfyAny rules) x
        == validate (verify $ foldr (</>) falseRule rules) x

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
    foldTest :: (Eq a, Eq e) => NonEmptyLQ (a -> Bool, e) -> a -> Bool
    foldTest predcs x = let rules = (\(f, err) -> predToVRule err f) <$> predcs
        in validate (verify $ satisfyAll rules) x
        == validate (verify $ fold rules) x
    leftFold1Test :: (Eq a, Eq e) => NonEmptyLQ (a -> Bool, e) -> a -> Bool
    leftFold1Test predcs x = let rules = (\(f, err) -> predToVRule err f) <$> predcs
        in validate (verify $ satisfyAll rules) x
        == validate (verify $ foldl1 (<>) rules) x
    rightFold1Test :: (Eq a, Eq e) => NonEmptyLQ (a -> Bool, e) -> a -> Bool
    rightFold1Test predcs x = let rules = (\(f, err) -> predToVRule err f) <$> predcs
        in validate (verify $ satisfyAll rules) x
        == validate (verify $ foldr1 (<>) rules) x
    leftFoldTest :: (Eq a, Eq e) => NonEmptyLQ (a -> Bool, e) -> a -> Bool
    leftFoldTest predcs x = let rules = (\(f, err) -> predToVRule err f) <$> predcs
        in validate (verify $ satisfyAll rules) x
        == validate (verify $ foldl (<>) mempty rules) x
    rightFoldTest :: (Eq a, Eq e) => NonEmptyLQ (a -> Bool, e) -> a -> Bool
    rightFoldTest predcs x = let rules = (\(f, err) -> predToVRule err f) <$> predcs
        in validate (verify $ satisfyAll rules) x
        == validate (verify $ foldr (<>) mempty rules) x

-- | Test ValidationRule combinators.
testCombs :: [TestTree]
testCombs =
  [ testGroup "Test primitive NonEmpty combinators" testPrimCombs
  , testGroup "Test primitive Unit combinators" testPrimCombs'
  , testGroup "Test primitive Unit combinators with labeled error" testPrimCombs''
  , testGroup "Test relationship between primitive NonEmpty and Unit combinators" testNEUnitRelation
  , testGroup "Test relationship between primitive 'if' and 'unless' combinators" testIfUnlessRelation
  ]

-- | Test functions that combine ValidationRules.
testCombMix :: [TestTree]
testCombMix =
  [ testGroup "Test `orElse` function" testOrElse
  , testGroup "Test `andAlso` function" testAndAlso
  , testGroup "Test `satisfyAny` function" testSatisfyAny
  , testGroup "Test `satisfyAll` function" testSatisfyAll
  ]

main :: IO ()
main = defaultMain $ testGroup "Test suite"
  [ testGroup "Test ValidationRule combinators" testCombs
  , testGroup "Test negateRule function" testNegateRule
  , testGroup "Test ValidationRule combining functions" testCombMix
  , testGroup "Test validation of a collection of Validators" testValidatorCollc
  , testGroup "Test Validation utilities" testValidationUtils
  ]
