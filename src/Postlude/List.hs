module Postlude.List (
    -- * Data Types
    List (..),

    -- * Basic Functions
    singleton,

    -- * Searching

    -- ** Searching With Predicate
    partition,
) where

import Postlude.Base
import Postlude.Bool (otherwise)
import Postlude.Semigroup

{- List -}
data List a
    = Nil
    | Cons a (List a)
    deriving (Show, Eq, Ord)

instance Semigroup (List a) where
    Nil <> ys = ys
    xs <> Nil = xs
    (Cons x xs) <> ys = Cons x (xs <> ys)

{- Functions -}
singleton :: a -> List a
singleton x = Cons x Nil

partition :: (a -> Bool) -> List a -> (List a, List a)
partition p =
    go
  where
    go = \case
        Nil -> (Nil, Nil)
        Cons x xs
            | p x -> (\(~(f, s)) -> (Cons x f, s)) (go xs)
            | otherwise -> (\(~(f, s)) -> (f, Cons x s)) (go xs)
