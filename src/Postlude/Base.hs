module Postlude.Base (
    -- * Base types
    module Data.Char,
    module Data.Bool,

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
import Data.Bool (Bool)
import Data.Char (Char, chr)

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
