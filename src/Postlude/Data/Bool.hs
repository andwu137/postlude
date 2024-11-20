module Postlude.Data.Bool (
    module Data.Bool,
    if',
    bool,
) where

import Data.Bool (
    Bool,
    otherwise,
 )

if' :: Bool -> a -> a -> a
if' b t f = if b then t else f

bool :: a -> a -> Bool -> a
bool f t b = if' b t f
