module Postlude.Applicative (
    Applicative (..),
) where

import Postlude.Apply
import Postlude.Pure

class (Apply f, Pure f) => Applicative f
