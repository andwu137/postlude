module Postlude.List (
    -- * Data Types
    List (..),

    -- * Basic Functions
    singleton,

    -- * Searching

    -- ** Searching With Predicate
    partition,

    -- * Sublists

    -- ** Extracting Sublists
    halve,

    -- * Joining
    mergeRuns,
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

mergeRuns :: (Ord a) => List a -> List a -> List a
mergeRuns = \cases
    xs Nil -> xs
    Nil ys -> ys
    (Cons x xs) (Cons y ys)
        | x < y -> Cons x (mergeRuns xs (Cons y ys))
        | otherwise -> Cons y (mergeRuns (Cons x xs) ys)

halve :: List a -> (List a, List a)
halve list =
    go list list
  where
    go = \cases
        ss Nil -> (Nil, ss)
        ss (Cons _ Nil) -> (Nil, ss)
        (Cons s ss) (Cons _ (Cons _ fs)) ->
            (\(~(h, t)) -> (Cons s h, t)) (go ss fs)
        Nil _ -> (Nil, Nil)

partition :: (a -> Bool) -> List a -> (List a, List a)
partition p =
    go
  where
    go = \case
        Nil -> (Nil, Nil)
        Cons x xs
            | p x -> (\(~(f, s)) -> (Cons x f, s)) (go xs)
            | otherwise -> (\(~(f, s)) -> (f, Cons x s)) (go xs)
