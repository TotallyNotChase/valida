{-# LANGUAGE Safe #-}

module Valida.ValidatorUtils
    ( validatorFrom
    ) where

import Data.Bool (bool)

import Valida.Validation (Validation (..))
import Valida.Validator  (Validator (Validator))

-- | Utility to convert a regular predicate function to a 'Validator'
validatorFrom :: (a -> Bool) -> e -> Validator e a ()
validatorFrom predc err = Validator $ bool (Failure err) (Success ()) . predc
