module Postlude.Data.ListZipper (
    ListZipper (..),
) where

import Postlude.Base
import Postlude.Class.Foldable
import Postlude.Class.Functor
import Postlude.Class.Pure
import qualified Postlude.Data.List as List

data ListZipper a
    = UnsafeListZipper
    { _leftList :: [a]
    , current :: a
    , _rightList :: [a]
    }
    deriving
        ( Show
        , Eq
        , Ord
        )

instance Functor ListZipper where
    map f (UnsafeListZipper{..}) =
        UnsafeListZipper
            { _leftList = f <$> _leftList
            , current = f current
            , _rightList = f <$> _rightList
            }

instance Pure ListZipper where
    pure x = UnsafeListZipper [] x []

instance Foldable ListZipper where
    foldr f d (UnsafeListZipper{..}) =
        let rs = foldr f d _rightList
            cs = f current rs
         in foldr f cs (List.reverse _leftList)
