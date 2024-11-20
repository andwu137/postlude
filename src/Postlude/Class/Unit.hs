module Postlude.Class.Unit (
    Unit (..),
) where

class Unit f where
    unit :: f a
