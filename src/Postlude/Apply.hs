module Postlude.Apply (
    Apply (..),
) where

import Postlude.Functor

class (Functor f) => Apply f where
    {-# MINIMAL (<*>) | liftA2 #-}

    infixl 4 <*>
    (<*>) :: f (a -> b) -> f a -> f b
    (<*>) = liftA2 (\x -> x)

    infixl 4 <#>
    (<#>) :: f a -> f (a -> b) -> f b
    x <#> f = (\y g -> g y) <$> x <*> f

    infixl 4 *>
    (*>) :: f a -> f b -> f b
    f *> g = (\x _ -> x) <$> g <*> f

    infixl 4 <*
    (<*) :: f a -> f b -> f a
    f <* g = (\x _ -> x) <$> f <*> g

    liftA2 :: (a -> b -> c) -> f a -> f b -> f c
    liftA2 f x y = f <$> x <*> y
