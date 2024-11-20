module Postlude.Data.List.ListF (
    ListF (..),
) where

import Postlude.Base
import Postlude.Class.Functor

{- ListF -}
data ListF a f
    = FNil
    | FCons a f
    deriving
        ( Show
        , Eq
        , Ord
        )

instance Functor (ListF a) where
    map f = \case
        FNil -> FNil
        FCons x xs -> FCons x (f xs)
