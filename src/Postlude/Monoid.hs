module Postlude.Monoid (
    Monoid,
) where

import Postlude.Empty
import Postlude.Semigroup

class (Empty a, Semigroup a) => Monoid a
