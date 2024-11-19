module Postlude.Monad (
    Monad (..),
) where

import Postlude.Apply
import Postlude.Functor
import Postlude.Pure

class (Apply f, Pure f) => Monad f where
    {-# MINIMAL (>>=) | join #-}

    infixl 1 >>=
    (>>=) :: f a -> (a -> f b) -> f b
    x >>= f = join (map f x)

    infixr 1 >=>
    (>=>) :: (a -> f b) -> (b -> f c) -> a -> f c
    (f >=> g) x = f x >>= g

    infixr 1 <=<
    (<=<) :: (b -> f c) -> (a -> f b) -> a -> f c
    (f <=< g) x = g x >>= f

    join :: f (f a) -> f a
    join x = x >>= \y -> y
