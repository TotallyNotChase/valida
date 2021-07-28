module Valida.Utils
    ( singleton
    ) where

import Data.List.NonEmpty (NonEmpty ((:|)))

singleton :: a -> NonEmpty a
singleton = (:|[])
