module Postlude.Class.Monoid (
    mconcat,
) where

import Postlude.Class.Empty
import Postlude.Class.Foldable
import Postlude.Class.Semigroup

mconcat :: (Foldable t, Empty a, Semigroup a) => t a -> a
mconcat = foldr (<>) empty
