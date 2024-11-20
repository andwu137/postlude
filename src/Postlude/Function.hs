module Postlude.Function (
    -- * Combinators

    -- ** Simple
    id,
    const,
    flip,
    (.),

    -- ** Advanced
    fix,
    on,

    -- * Function application
    ($),
    (&),
    applyWhen,
) where

import Postlude.Base

id :: a -> a
id x = x

const :: a -> b -> a
const x _ = x

flip :: (a -> b -> c) -> b -> a -> c
flip f x y = f y x

-- fix f = f ~(fix f)
fix :: (a -> a) -> a
fix f = let x = f x in x

on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
(f `on` g) x y = f (g x) (g y)

infixr 0 $
($) :: (a -> b) -> a -> b
($) = id

infixl 1 &
(&) :: a -> (a -> b) -> b
(&) = flip ($)

infixr 9 .
(.) :: (b -> c) -> (a -> b) -> a -> c
(f . g) x = f (g x)

applyWhen :: Bool -> (a -> a) -> a -> a
applyWhen b = if b then id else const id
