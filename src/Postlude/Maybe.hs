module Postlude.Maybe (
    -- * Data Type
    Maybe (..),

    -- * Extracting Values
    maybe,
    fromMaybe,
) where

import Postlude.Alternative
import Postlude.Apply
import Postlude.Base
import Postlude.Empty
import Postlude.Foldable
import Postlude.Functor
import Postlude.Monad
import Postlude.Monoid
import Postlude.Pure
import Postlude.Semigroup
import Postlude.Unit

data Maybe a
    = Nothing
    | Just a
    deriving
        ( Show
        , Eq
        , Ord
        )

instance Functor Maybe where
    map f = \case
        Nothing -> Nothing
        Just x -> Just (f x)

instance Apply Maybe where
    mf <*> x = case mf of
        Nothing -> Nothing
        Just f -> map f x

instance Unit Maybe where
    unit = Nothing

instance Pure Maybe where
    pure = Just

instance Monad Maybe where
    mx >>= f = maybe Nothing f mx

instance Alternative Maybe where
    x <|> y = case x of
        Nothing -> y
        Just{} -> x

instance Foldable Maybe where
    foldr f d = \case
        Nothing -> d
        Just x -> f x d

instance Empty (Maybe a) where
    empty = Nothing

instance (Semigroup a) => Semigroup (Maybe a) where
    Nothing <> b = b
    a <> Nothing = a
    Just a <> Just b = Just (a <> b)

instance (Semigroup a) => Monoid (Maybe a)

maybe :: b -> (a -> b) -> Maybe a -> b
maybe d f = \case
    Nothing -> d
    Just x -> f x

fromMaybe :: b -> Maybe b -> b
fromMaybe d = maybe d (\x -> x)
