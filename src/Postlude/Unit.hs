module Postlude.Unit (
    Unit (..),
) where

class Unit f where
    unit :: f a
