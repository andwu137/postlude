module Postlude.Semigroup (
    Semigroup (..),
) where

class Semigroup a where
    (<>) :: a -> a -> a
