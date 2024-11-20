module Postlude.Class.FromList (
    FromList (..),
) where

class FromList t a where
    fromList :: [a] -> t a
