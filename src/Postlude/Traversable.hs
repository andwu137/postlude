module Postlude.Traversable (
    Traversable (..),
) where

import Postlude.Apply
import Postlude.Foldable
import Postlude.Functor
import Postlude.Pure

class (Functor t, Foldable t) => Traversable t where
    {-# MINIMAL traverse | sequence #-}
    traverse :: (Apply f, Pure f) => (a -> f b) -> t a -> f (t b)
    traverse f xs = sequence (f <$> xs)

    sequence :: (Apply f, Pure f) => t (f a) -> f (t a)
    sequence = traverse (\x -> x)
