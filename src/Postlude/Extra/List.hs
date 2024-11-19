module Postlude.Extra.List (
    uncons,
) where

import qualified Postlude.Maybe as Maybe
import qualified Postlude.NonEmpty as NonEmpty

uncons :: [a] -> Maybe.Maybe (NonEmpty.NonEmpty a)
uncons = \case
    [] -> Maybe.Nothing
    x : xs -> Maybe.Just (x NonEmpty.:| xs)
