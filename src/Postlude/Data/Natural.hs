module Postlude.Data.Natural (
    Natural (..),
    makeNatural,
) where

import Postlude.Base
import Postlude.Data.Maybe

newtype Natural
    = UnsafeNatural
    { getNatural :: Int
    }
    deriving
        ( Show
        , Eq
        , Ord
        )

makeNatural :: Int -> Maybe Natural
makeNatural n
    | n < 0 = Nothing
    | otherwise = Just (UnsafeNatural n)
