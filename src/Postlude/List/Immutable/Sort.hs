module Postlude.List.Immutable.Sort (
    quickSort,
    mergeSort,
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

mergeSort :: (Ord a) => List a -> List a
mergeSort = \case
    Nil -> Nil
    Cons x Nil -> Cons x Nil
    xs ->
        let (l, r) = halve xs
         in mergeRuns (mergeSort l) (mergeSort r)
