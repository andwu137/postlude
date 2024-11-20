module Postlude.Class.Bifunctor (
    BiFunctor (..),
) where

class BiFunctor f where
    {-# MINIMAL bimap | (first, second) #-}
    bimap :: (a -> c) -> (b -> d) -> f a b -> f c d
    bimap f g x = first f (second g x)

    first :: (a -> c) -> f a b -> f c b
    first f = bimap f (\x -> x)

    second :: (b -> d) -> f a b -> f a d
    second g = bimap (\x -> x) g

    both :: (a -> b) -> f a a -> f b b
    both f = bimap f f
