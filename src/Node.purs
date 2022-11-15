module Node where

import Prelude (class Eq, class Show)

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

import NodeID (NodeID)
import Handle (Handle)

data Nature = Reference | Definition

derive instance eqNature :: Eq Nature
derive instance genNature :: Generic Nature _
instance Show Nature where show = genericShow

data Scoping = Scoped | Unscoped

derive instance eqScoping :: Eq Scoping
derive instance genScoping :: Generic Scoping _
instance Show Scoping where show = genericShow

data Visibility = Exported | Internal

derive instance eqVisibility :: Eq Visibility
derive instance genVisibility :: Generic Visibility _
instance Show Visibility where show = genericShow

type DropScopesNode = { ident :: NodeID }

type SymbolNode = { ident :: NodeID, symbol :: Handle String, scoping :: Scoping, nature :: Nature }

type ScopeNode = { ident :: NodeID, visibility :: Visibility }

data Node
  = Root
  | JumpTo
  | Push SymbolNode
  | Pop SymbolNode
  | Scope ScopeNode
  | Drop DropScopesNode
