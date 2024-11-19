module Postlude.Either (
    Either (..),
    either,
) where

import Postlude.Apply
import Postlude.Base
import Postlude.Bifunctor
import Postlude.Error
import Postlude.Foldable
import Postlude.Functor
import Postlude.Monad
import Postlude.Pure
import Postlude.Semigroup
import Postlude.Traversable

data Either e a
    = Left e
    | Right a
    deriving
        ( Show
        , Eq
        , Ord
        )

instance Functor (Either e) where
    map f = \case
        Left e -> Left e
        Right x -> Right (f x)

instance BiFunctor Either where
    bimap f g = \case
        Left e -> Left (f e)
        Right x -> Right (g x)

instance Pure (Either e) where
    pure = Right

instance Error e (Either e) where
    throwError = Left
    catchError x f = case x of
        Left e -> f e
        _ -> x

instance Apply (Either e) where
    (Left e) <*> _ = Left e
    _ <*> (Left e) = Left e
    (Right f) <*> (Right x) = Right (f x)

instance Monad (Either e) where
    mx >>= f = case mx of
        Left e -> Left e
        Right x -> f x

instance Foldable (Either e) where
    foldr f d = \case
        Left _ -> d
        Right x -> f x d

instance Traversable (Either e) where
    traverse f = \case
        Left e -> pure (Left e)
        Right x -> Right <$> f x

instance Semigroup (Either e a) where
    Left _ <> b = b
    a <> _ = a

either :: (a -> c) -> (b -> d) -> Either a b -> Either c d
either = bimap
