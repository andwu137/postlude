module Postlude.Extra.List (
    uncons,
) where

import Postlude.List
import qualified Postlude.Maybe as Maybe
import qualified Postlude.NonEmpty as NonEmpty

uncons :: List a -> Maybe.Maybe (NonEmpty.NonEmpty a)
uncons = \case
    Nil -> Maybe.Nothing
    Cons x xs -> Maybe.Just (NonEmpty.NonEmpty x xs)
