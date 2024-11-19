module Postlude.Semigroup (
    Semigroup (..),
) where

import Postlude.NonEmpty as NonEmpty

class Semigroup a where
    {-# MINIMAL (<>) | sconcat #-}
    (<>) :: a -> a -> a
    a <> b = sconcat (a NonEmpty.:| [b])

    sconcat :: NonEmpty.NonEmpty a -> a
    sconcat (a NonEmpty.:| as) =
        go a as
      where
        go b (c : cs) = b <> go c cs
        go b [] = b
