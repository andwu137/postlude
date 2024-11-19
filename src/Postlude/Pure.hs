module Postlude.Pure (
    Pure (..),
) where

class Pure f where
    pure :: a -> f a
