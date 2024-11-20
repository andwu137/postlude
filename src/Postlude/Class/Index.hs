module Postlude.Class.Index (
    Index (..),
) where

import Postlude.Base
import Postlude.Data.Maybe

class Index f where
    {-# MINIMAL index #-}
    index :: Int -> f a -> Maybe a

    indexUnsafe :: (HasCallStack) => Int -> f a -> a
    indexUnsafe n xs = let (Just x) = index n xs in x

    (!) :: f a -> Int -> Maybe a
    xs ! n = index n xs

    (!!) :: f a -> Int -> a
    xs !! n = indexUnsafe n xs
