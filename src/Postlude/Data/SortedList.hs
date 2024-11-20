module Postlude.Data.SortedList (
    SortedList (..),
) where

import Postlude.Base
import Postlude.Class.Empty
import Postlude.Class.Foldable
import Postlude.Class.Functor
import Postlude.Class.Index
import Postlude.Class.Pure
import Postlude.Class.Semigroup
import Postlude.Class.Unit
import Postlude.Data.List ()
import qualified Postlude.Data.List as List
import qualified Postlude.Data.List.Immutable.Sort as List.Sort

newtype SortedList a
    = UnsafeSortedList [a]
    deriving
        ( Show
        , Eq
        , Ord
        , Functor
        , Foldable
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
