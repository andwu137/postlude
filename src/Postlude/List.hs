module Postlude.List (
    List (..),
    ListF (..),
) where

import Postlude.Base
import Postlude.Functor

data List a
    = Nil
    | Cons a (List a)
    deriving (Show, Eq, Ord)

data ListF a f
    = FNil
    | FCons a f
    deriving (Show, Eq, Ord)

instance Functor (ListF a) where
    map f = \case
        FNil -> FNil
        FCons x xs -> FCons x (f xs)
