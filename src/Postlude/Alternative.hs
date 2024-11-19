module Postlude.Alternative (
    Alternative (..),
) where

import Postlude.Apply
import Postlude.Foldable
import Postlude.Functor
import qualified Postlude.List as List
import Postlude.Pure
import Postlude.Unit

class (Unit f, Pure f, Apply f) => Alternative f where
    {-# MINIMAL (<|>) #-}

    infixl 3 <|>
    (<|>) :: f a -> f a -> f a

    some :: f a -> f (List.List a)
    some x = List.Cons <$> x <*> many x

    many :: f a -> f (List.List a)
    many x = (List.Cons <$> x <*> many x) <|> pure List.Nil

    asum :: (Foldable t) => t (f a) -> f a
    asum = foldr (<|>) unit
