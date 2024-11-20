module Postlude.Class.Monad (
    Monad (..),
) where

import Postlude.Class.Apply
import Postlude.Class.Functor
import Postlude.Class.Pure

class (Apply f, Pure f) => Monad f where
    {-# MINIMAL (>>=) | join #-}

    infixl 1 >>=
    (>>=) :: f a -> (a -> f b) -> f b
    x >>= f = join (map f x)

    infixr 1 =<<
    (=<<) :: (a -> f b) -> f a -> f b
    f =<< x = join (map f x)

    infixr 1 >=>
    (>=>) :: (a -> f b) -> (b -> f c) -> a -> f c
    (f >=> g) x = f x >>= g

    infixr 1 <=<
    (<=<) :: (b -> f c) -> (a -> f b) -> a -> f c
    (f <=< g) x = g x >>= f

    join :: f (f a) -> f a
    join x = x >>= \y -> y
