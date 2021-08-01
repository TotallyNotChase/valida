{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module SeriesInst
    ( ValidationS (..)
    ) where

import Test.SmallCheck.Series (Serial (..), cons1, (\/))

import Valida (Validation (..))

newtype ValidationS e a = ValdS (Validation e a) deriving (Show)

instance (Serial m e, Serial m a) => Serial m (ValidationS e a) where
    series = cons1 (ValdS . Failure) \/ cons1 (ValdS . Success)
