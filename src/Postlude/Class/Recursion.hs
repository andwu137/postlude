{- Taken *heavy* inspiration from recursion-schemes
 - `https://hackage.haskell.org/package/recursion-schemes`
 -}

module Postlude.Class.Recursion (
    PType,
    Recursive (..),
    Corecursive (..),
    hylo,
) where

import Postlude.Base (Type)
import Postlude.Class.Functor
import Postlude.Function

type family PType t :: (Type -> Type)

class (Functor (PType t)) => Recursive t where
    {-# MINIMAL project #-}
    project :: t -> PType t t

    cata :: (PType t a -> a) -> t -> a
    cata f =
        let c = f . map c . project
         in c

class (Functor (PType t)) => Corecursive t where
    {-# MINIMAL embed #-}
    embed :: PType t t -> t

    ana :: (a -> PType t a) -> a -> t
    ana f =
        let a = embed . map a . f
         in a

hylo :: (Functor f) => (f b -> b) -> (a -> f a) -> a -> b
hylo f g = h where h = f . map h . g
