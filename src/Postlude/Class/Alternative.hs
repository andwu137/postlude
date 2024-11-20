module Postlude.Class.Alternative (
    Alternative (..),
) where

import Postlude.Class.Apply
import Postlude.Class.Foldable
import Postlude.Class.Functor
import Postlude.Class.Pure
import Postlude.Class.Semigroup
import Postlude.Class.Unit

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
