module Postlude.SortedList (
    SortedList (..),
) where

import Postlude.Base
import Postlude.Empty
import Postlude.Functor
import Postlude.Index
import Postlude.List ()
import qualified Postlude.List as List
import qualified Postlude.List.Immutable.Sort as List.Sort
import Postlude.Pure
import Postlude.Semigroup
import Postlude.Unit

newtype SortedList a
    = UnsafeSortedList [a]
    deriving
        ( Show
        , Eq
        , Ord
        , Functor
        , Pure
        , Empty
        , Unit
        )

instance (Ord a) => Semigroup (SortedList a) where
    (UnsafeSortedList xs) <> (UnsafeSortedList ys) =
        UnsafeSortedList (List.mergeRuns xs ys)

instance Index SortedList where
    index n (UnsafeSortedList xs) = index n xs

instance (Ord a) => List.FromList SortedList a where
    fromList xs = UnsafeSortedList (List.Sort.mergeSort xs)
