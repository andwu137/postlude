module Postlude.Extra.List (
    uncons,
) where

import qualified Postlude.Data.Maybe as Maybe
import qualified Postlude.Data.NonEmpty as NonEmpty

uncons :: [a] -> Maybe.Maybe (NonEmpty.NonEmpty a)
uncons = \case
    [] -> Maybe.Nothing
    x : xs -> Maybe.Just (x NonEmpty.:| xs)
