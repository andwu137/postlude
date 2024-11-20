{-# OPTIONS_GHC -Wno-orphans #-}

module Postlude.Base (
    -- * Base types
    module Data.Bool,
    module Data.Char,
    module Data.Int,
    Integer,

    -- * Base type classes
    module Data.Eq,
    module Data.Ord,

    -- * System IO
    module System.IO,

    -- * Types for type-level computation
    module Data.Coerce,
    module Data.Kind,
    module Data.Type.Equality,
    module Data.Typeable,
    module Data.Void,

    -- * Basic type classes
    module GHC.Base,
    module GHC.Generics,
    module GHC.Show,

    -- * GHC-specific functionality
    module GHC.TypeNats,
    module GHC.OverloadedLabels,
    module GHC.ExecutionStack,
    module GHC.Stack,
) where

-- Base types
import Data.Bool (
    Bool,
    otherwise,
 )
import Data.Char (
    Char,
    chr,
 )

import Data.Int
import Postlude.Class.Numeric
import Prelude (Integer)
import qualified Prelude

-- IO
import System.IO (FilePath, IO, IOMode (..))

-- Base typeclasses
import Data.Eq (Eq, (/=), (==))
import Data.Ord (Down (..), Ord (..), Ordering (..), comparing)

-- Types for type-level computation
import Data.Coerce (Coercible, coerce)
import Data.Kind (Constraint, Type)
import Data.Type.Equality (type (~))
import Data.Typeable (Typeable)
import Data.Void (Void, absurd, vacuous)

import GHC.Base (asTypeOf, ord, seq, ($!))
import GHC.Generics (Generic)
import GHC.Show (Show)

import GHC.TypeNats (CmpNat, KnownNat, Nat, SomeNat (..), natVal, someNatVal)

import GHC.ExecutionStack (getStackTrace, showStackTrace)
import GHC.OverloadedLabels (IsLabel (..))
import GHC.Stack (
    CallStack,
    HasCallStack,
    callStack,
    currentCallStack,
    getCallStack,
    prettyCallStack,
    prettySrcLoc,
    withFrozenCallStack,
 )

-- Base Type Instances
{- Int -}
instance Add Int where
    (+) = (Prelude.+)

instance Sub Int where
    (-) = (Prelude.-)

instance Negate Int where
    negate = Prelude.negate

instance Mul Int where
    (*) = (Prelude.*)

instance DivRound Int where
    (//) = Prelude.div

{- Integer -}
instance Add Integer where
    (+) = (Prelude.+)

instance Sub Integer where
    (-) = (Prelude.-)

instance Negate Integer where
    negate = Prelude.negate

instance Mul Integer where
    (*) = (Prelude.*)

instance DivRound Integer where
    (//) = Prelude.div
