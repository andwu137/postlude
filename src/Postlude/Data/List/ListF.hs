module Postlude.Data.List.ListF (

) where

import Postlude.Base
import Postlude.Class.Foldable
import Postlude.Class.Functor
import Postlude.Class.Pure
import Postlude.Class.Recursion
import Postlude.Class.Traversable

{- Recursion Schemes -}
data ListF a f
    = NilF
    | ConsF a f
    deriving
        ( Show
        , Eq
        , Ord
        )

instance Functor (ListF a) where
    map f = \case
        NilF -> NilF
        ConsF x xs -> ConsF x (f xs)

instance Foldable (ListF a) where
    foldr f d = \case
        NilF -> d
        ConsF _ y -> f y d

instance Traversable (ListF a) where
    traverse f = \case
        NilF -> pure NilF
        ConsF x y -> ConsF x <$> f y

type instance PType [a] = ListF a
instance Recursive [a] where
    project = \case
        [] -> NilF
        x : xs -> ConsF x xs
instance Corecursive [a] where
    embed = \case
        NilF -> []
        ConsF x xs -> x : xs
