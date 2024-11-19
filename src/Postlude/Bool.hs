module Postlude.Bool (
    if',
    bool,
) where

import Postlude.Base

if' :: Bool -> a -> a -> a
if' b t f = if b then t else f

bool :: a -> a -> Bool -> a
bool f t b = if' b t f
