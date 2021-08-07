module Valida.ValidationUtils
    ( fromEither
    , toEither
    ) where

import Valida.Validation (Validation (..), validation)

{- | Convert a 'Validation' to an 'Either'.

Given, __Validation a b__-

  * __Failure a__ is converted to __Left a__.
  * __Success b__ is converted to __Right b__.
-}
toEither :: Validation a b -> Either a b
toEither = validation Left Right

{- | Convert a 'Either' to an 'Validation'.

Given, __Either e a__-

  * __Left e__ is converted to __Failure e__.
  * __Right a__ is converted to __Success a__.
-}
fromEither :: Either e a -> Validation e a
fromEither = either Failure Success
