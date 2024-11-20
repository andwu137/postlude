module Postlude.Class.Traversable (
    Traversable (..),
) where

import Postlude.Class.Apply
import Postlude.Class.Foldable
import Postlude.Class.Functor
import Postlude.Class.Pure

class (Functor t, Foldable t) => Traversable t where
    {-# MINIMAL traverse | sequence #-}
    traverse :: (Apply f, Pure f) => (a -> f b) -> t a -> f (t b)
    traverse f xs = sequence (f <$> xs)

    sequence :: (Apply f, Pure f) => t (f a) -> f (t a)
    sequence = traverse (\x -> x)
