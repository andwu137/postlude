module Postlude.Monad (
    Monad (..),
) where

import Postlude.Apply
import Postlude.Functor
import Postlude.Pure
import Postlude.Unit

class (Apply f, Pure f) => Monad f where
    {-# MINIMAL (>>=) #-}

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

    void :: f a -> f ()
    void = map (\_ -> ())

    unzip :: f (a, b) -> (f a, f b)
    unzip xs = ((\(x, _) -> x) <$> xs, (\(_, x) -> x) <$> xs)
