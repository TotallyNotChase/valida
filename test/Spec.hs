module Main
    ( main
    ) where

import Data.Bool   (bool)
import Data.Either (isRight)
import Data.Maybe  (isNothing)

import           Test.Tasty            (TestTree, defaultMain, testGroup)
import           Test.Tasty.HUnit      (testCase, (@?=))
import qualified Test.Tasty.QuickCheck as QC
import qualified Test.Tasty.SmallCheck as SC

import Valida (Validation (..), ValidationRule, Validator (validate), failureIf, failureIf', failureUnless,
               failureUnless', negateRule, negateRule', verify, vrule, (-?>), (<?>))

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

-- | Test ValidationRule combinators.
testCombs :: [TestTree]
testCombs =
  [ testGroup "Test primitive NonEmpty combinators" testPrimCombs
  , testGroup "Test primitive Unit combinators" testPrimCombs'
  , testGroup "Test primitive Unit combinators with labeled error" testPrimCombs''
  , testGroup "Test relationship between primitive NonEmpty and Unit combinators" testNEUnitRelation
  , testGroup "Test relationship between primitive 'if' and 'unless' combinators" testIfUnlessRelation
  ]

main :: IO ()
main = defaultMain $ testGroup "Test suite"
  [ testGroup "Test ValidationRule combinators" testCombs
  , testGroup "Test validation of a collection of Validators" testValidatorCollc
  ]
