module Postlude.Empty (
    Empty (..),
) where

class Empty a where
    empty :: a
