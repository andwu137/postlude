module Postlude.Natural (
    Natural (..),
    makeNatural,
) where

import Data.Bool
import Postlude.Base
import Postlude.Int
import Postlude.Maybe

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
