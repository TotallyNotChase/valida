{-# LANGUAGE Safe #-}

module Valida.Utils
    ( neSingleton
    ) where

import Data.List.NonEmpty (NonEmpty ((:|)))

neSingleton :: a -> NonEmpty a
neSingleton = (:|[])
