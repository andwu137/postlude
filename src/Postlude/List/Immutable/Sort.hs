module Postlude.List.Immutable.Sort (
    quickSort,
    mergeSort,
) where

import Postlude.Base
import Postlude.List
import Postlude.Semigroup

quickSort :: (Ord a) => [a] -> [a]
quickSort = \case
    [] -> []
    x : xs ->
        let (ls, rs) = partition (< x) xs
         in quickSort ls <> singleton x <> quickSort rs

mergeSort :: (Ord a) => [a] -> [a]
mergeSort = \case
    [] -> []
    [x] -> [x]
    xs ->
        let (l, r) = halve xs
         in mergeRuns (mergeSort l) (mergeSort r)
