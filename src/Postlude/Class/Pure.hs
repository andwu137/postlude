module Postlude.Class.Pure (
    Pure (..),
) where

class Pure f where
    pure :: a -> f a
