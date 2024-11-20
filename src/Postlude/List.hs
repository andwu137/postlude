{-# OPTIONS_GHC -Wno-orphans #-}

module Postlude.List (
    -- * Basic Functions
    singleton,

    -- * Conversions
    toList,
    FromList (..),

    -- * Transformations
    reverse,

    -- * Building

    -- * Searching

    -- ** Searching With Predicate
    partition,
    partitionOrd,

    -- * Sublists

    -- ** Extracting Sublists
    halve,

    -- * Joining
    mergeRuns,
) where

import Postlude.Alternative
import Postlude.Apply
import Postlude.Base
import Postlude.Bool
import Postlude.Empty
import Postlude.Enum
import Postlude.Foldable
import Postlude.Functor
import Postlude.Index
import Postlude.Maybe
import Postlude.Monad
import Postlude.Pure
import Postlude.Semigroup
import Postlude.Traversable
import Postlude.Unit

class FromList t a where
    fromList :: [a] -> t a

toList :: (Foldable t) => t a -> [a]
toList = foldr (:) []

instance Functor [] where
    map f = \case
        [] -> []
        x : xs -> f x : map f xs

instance Apply [] where
    [] <*> _ = []
    _ <*> [] = []
    (f : fs) <*> xs = (f <$> xs) <> (fs <*> xs)

instance Pure [] where
    pure x = [x]

instance Monad [] where
    join = concat

instance Unit [] where
    unit = []

instance Alternative [] where
    (<|>) = (<>)

instance Semigroup [a] where
    [] <> ys = ys
    xs <> [] = xs
    (x : xs) <> ys = x : (xs <> ys)

instance Empty [a] where
    empty = []

instance Index [] where
    index n
        | n < 0 = \_ -> Nothing
        | otherwise = \case
            [] -> Nothing
            x : xs
                | n == 0 -> Just x
                | otherwise -> index (pred n) xs

instance Traversable [] where
    traverse f =
        go
      where
        go = \case
            [] -> pure []
            x : xs -> (:) <$> f x <*> go xs

{- Functions -}
singleton :: a -> [a]
singleton = pure

reverse :: [a] -> [a]
reverse = foldl (\x y -> y : x) []

mergeRuns :: (Ord a) => [a] -> [a] -> [a]
mergeRuns = \cases
    xs [] -> xs
    [] ys -> ys
    (x : xs) (y : ys)
        | x < y -> x : mergeRuns xs (y : ys)
        | otherwise -> y : mergeRuns (x : xs) ys

halve :: [a] -> ([a], [a])
halve list =
    go list list
  where
    go = \cases
        ss [] -> ([], ss)
        ss [_] -> ([], ss)
        (s : ss) (_ : _ : fs) ->
            let (h, t) = go ss fs
             in (s : h, t)
        [] _ -> ([], [])

partition :: (a -> Bool) -> [a] -> ([a], [a])
partition p =
    go
  where
    go = \case
        [] -> ([], [])
        x : xs ->
            let (f, s) = go xs
             in if p x
                    then (x : f, s)
                    else (f, x : s)

partitionOrd :: (a -> Ordering) -> [a] -> ([a], [a], [a])
partitionOrd p =
    go
  where
    go = \case
        [] -> ([], [], [])
        x : xs ->
            let (lt, eq, gt) = go xs
             in case p x of
                    LT -> (x : lt, eq, gt)
                    EQ -> (lt, x : eq, gt)
                    GT -> (lt, eq, x : gt)
