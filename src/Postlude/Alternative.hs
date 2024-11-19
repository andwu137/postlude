module Postlude.Alternative (
    Alternative (..),
) where

import Postlude.Apply
import Postlude.Foldable
import Postlude.Functor
import Postlude.Pure
import Postlude.Semigroup
import Postlude.Unit

class (Unit f, Pure f, Apply f) => Alternative f where
    {-# MINIMAL (<|>) #-}

    infixl 3 <|>
    (<|>) :: f a -> f a -> f a

    some :: f a -> f [a]
    some x = (:) <$> x <*> many x

    many :: f a -> f [a]
    many x = ((:) <$> x <*> many x) <|> pure []

    asum :: (Foldable t) => t (f a) -> f a
    asum = foldr (<|>) unit

    concat :: (Foldable f, Semigroup (f a)) => f (f a) -> f a
    concat = foldr (<>) unit
