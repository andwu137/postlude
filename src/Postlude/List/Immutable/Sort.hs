module Postlude.List.Immutable.Sort (
    quicksort,
) where

import Postlude.Base
import Postlude.List
import Postlude.Semigroup

quicksort :: (Ord a) => List a -> List a
quicksort = \case
    Nil -> Nil
    Cons x xs ->
        let (ls, rs) = partition (<= x) xs
         in quicksort ls <> singleton x <> quicksort rs
