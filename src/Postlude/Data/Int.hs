module Postlude.Data.Int (
    module Data.Int,
) where

import Data.Int
import Postlude.Class.Numeric
import qualified Prelude

instance Add Int where
    (+) = (Prelude.+)

instance Sub Int where
    (-) = (Prelude.-)

instance Negate Int where
    negate = Prelude.negate

instance Mul Int where
    (*) = (Prelude.*)

instance DivRound Int where
    (//) = Prelude.div
