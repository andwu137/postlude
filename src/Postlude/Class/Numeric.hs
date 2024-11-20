module Postlude.Class.Numeric (
    Add (..),
    Sub (..),
    Negate (..),
    Mul (..),
    DivRound (..),
    DivFull (..),
    Number (..),
    Fractional (..),
) where

import Postlude.Data.Integer

class Add a where
    infixl 6 +
    (+) :: a -> a -> a

class Sub a where
    infixl 6 -
    (-) :: a -> a -> a

class (Sub a) => Negate a where
    negate :: a -> a

class Mul a where
    infixl 7 *
    (*) :: a -> a -> a

class DivRound a where
    infixl 7 //
    (//) :: a -> a -> a

class DivFull a where
    infixl 7 /
    (/) :: a -> a -> a

class (Add n, Sub n, Mul n) => Number n where
    fromInteger :: Integer -> n

class (Number n, DivFull n) => Fractional n where
    recip :: n -> n

-- class (Fractional n) => Floating n where
-- class Integral n where
-- class Real n where
