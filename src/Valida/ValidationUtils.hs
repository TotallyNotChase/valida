module Valida.ValidationUtils
    ( fromEither
    , toEither
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

{- | Convert a 'Either' to an 'Validation'.

Given, __Either a b__-

  * __Left a__ is converted to __Failure a__.
  * __Right b__ is converted to __Success b__.
-}
fromEither :: Either a b -> Validation a b
fromEither (Left e)  = Failure e
fromEither (Right a) = Success a
