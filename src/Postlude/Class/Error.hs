module Postlude.Class.Error (
    Error (..),
) where

class Error e f | f -> e where
    throwError :: e -> f a
    catchError :: f a -> (e -> f a) -> f a
