module Postlude.NonEmpty (
    NonEmpty (..),
    head,
    tail,
) where

import Postlude.Base

data NonEmpty a
    = a :| [a]
    deriving (Show, Eq, Ord)

head :: NonEmpty a -> a
head (x :| _) = x

tail :: NonEmpty a -> [a]
tail (_ :| xs) = xs
