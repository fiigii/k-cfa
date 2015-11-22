module ControlFlow where

import LabeledAst
import Data.Set (Set, union, singleton, isSubsetOf, member)
import qualified Data.Set as Set
import Data.Map (Map, findWithDefault)
import qualified Data.Map as Map
import Data.List (partition)
import Ast (Ast)

data Abstract = Cache Label
              | Envir String
              deriving (Eq, Ord, Show)

data Constraint = Concrete LAst Abstract
                | Subset Abstract Abstract
                | Conditional LAst Abstract Abstract Abstract
                deriving (Eq, Ord)

type AbstractValue = [(LAst, ContextEnvironment)]
type Cache = Map (Label, Context) AbstractValue
type Envir = Map (String, Context) AbstractValue
