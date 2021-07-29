module Valida.ValidationUtils
    ( toEither
    ) where

import Valida.Validation (Validation (..))

{- | Convert a 'Validation' to an 'Either'.

Given, __Validation a b__-

  * __Failure a__ is converted to __Left a__.
  * __Success b__ is converted to __Right b__.
-}
toEither :: Validation a b -> Either a b
toEither (Failure e) = Left e
toEither (Success a) = Right a
