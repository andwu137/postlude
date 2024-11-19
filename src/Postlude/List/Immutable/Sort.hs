module Postlude.List.Immutable.Sort (
    quickSort,
) where

import Postlude.Base
import Postlude.List
import Postlude.Semigroup

quickSort :: (Ord a) => List a -> List a
quickSort = \case
    Nil -> Nil
    Cons x xs ->
        let (ls, rs) = partition (<= x) xs
         in quickSort ls <> singleton x <> quickSort rs
