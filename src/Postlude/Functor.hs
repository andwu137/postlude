module Postlude.Functor (
    Functor (..),
    (<<$>>),
    (<&>),
) where

class Functor f where
    {-# MINIMAL map #-}

    map :: (a -> b) -> f a -> f b
    map = (<$>)

    infixl 4 <$>
    (<$>) :: (a -> b) -> f a -> f b
    (<$>) = map

    infixl 4 $>
    (<$) :: a -> f b -> f a
    x <$ f = map (\_ -> x) f

    infixl 4 <$
    ($>) :: f a -> b -> f b
    f $> x = map (\_ -> x) f

    void :: f a -> f ()
    void = map (\_ -> ())

    unzip :: f (a, b) -> (f a, f b)
    unzip xs = ((\(x, _) -> x) <$> xs, (\(_, x) -> x) <$> xs)

instance Functor ((->) a) where
    map f g x = f (g x)

infixl 4 <<$>>
(<<$>>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<<$>>) = map map map

infixl 1 <&>
(<&>) :: (Functor f) => f a -> (a -> b) -> f b
x <&> f = map f x
