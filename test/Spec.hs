module Main
    ( main
    ) where

import Data.List.NonEmpty (NonEmpty)

import Test.Tasty            (TestTree, defaultMain, testGroup)
import Test.Tasty.SmallCheck (testProperty)

import SeriesInst (ValidationS (..))

{-# ANN validationInstanceProp "HLINT: ignore Functor law" #-}
{-# ANN validationInstanceProp "HLINT: ignore Redundant id" #-}
-- | Property tests for typeclass instances of 'Validation'.
validationInstanceProp :: [TestTree]
validationInstanceProp =
  [ testGroup "Functor law tests for `Validation`"
    [ testGroup "fmap id == id (Identity law)"
      [ testProperty "e = String, a = String" (fmapId :: ValidationS String String -> Bool)
      , testProperty "e = NonEmpty String, a = Int"
            (fmapId :: ValidationS (NonEmpty String) Int -> Bool)
      , testProperty "e = (), a = Bool" (fmapId :: ValidationS () Bool -> Bool)
      , testProperty "e = [Int], a = ()" (fmapId :: ValidationS [Int] () -> Bool)
      ]
    ]
  , testGroup "fmap (f . g) == fmap f . fmap g (Composition law)"
      [ testProperty "e = String, a = Bool, b = Char, c = ()"
            (fmapComp
              :: ValidationS String Bool
              -> (Bool -> Char)
              -> (Char -> ())
              -> Bool
            )
      , testProperty "e = NonEmpty String, a = Int, b = (), c = Maybe Bool"
            (fmapComp
              :: ValidationS (NonEmpty String) ()
              -> (() -> ())
              -> (() -> Maybe Bool)
              -> Bool
            )
      ]
  ]
  where
    fmapId :: (Eq e,Eq a) => ValidationS e a -> Bool
    fmapId = \(ValdS val) -> fmap id val == id val
    fmapComp
      :: (Eq e, Eq c)
      => ValidationS e a
      -> (a -> b)
      -> (b -> c)
      -> Bool
    fmapComp = \(ValdS val) g f -> fmap (f . g) val == (fmap f . fmap g $ val)

main :: IO ()
main = defaultMain $ testGroup "Test Suite"
  [ testGroup "Property tests for `Validation` typeclass instances" validationInstanceProp
  ]
