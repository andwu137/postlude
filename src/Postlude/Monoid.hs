module Postlude.Monoid (
    mconcat,
) where

import Postlude.Empty
import Postlude.Foldable
import Postlude.Semigroup

mconcat :: (Foldable t, Empty a, Semigroup a) => t a -> a
mconcat = foldr (<>) empty
