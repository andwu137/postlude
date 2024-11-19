module Postlude.NonEmpty (
    NonEmpty (..),
) where

import Postlude.Base

data NonEmpty a
    = NonEmpty
    { head :: a
    , tail :: [a]
    }
    deriving (Show, Eq, Ord)
