module Postlude.NonEmpty (
    NonEmpty (..),
) where

import Postlude.Base
import qualified Postlude.List as List

data NonEmpty a
    = NonEmpty
    { head :: a
    , tail :: List.List a
    }
    deriving (Show, Eq, Ord)
