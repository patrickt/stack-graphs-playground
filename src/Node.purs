module Node where

import Prelude
import Prim hiding (Symbol)

import Data.Maybe
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

import NodeID (NodeID)
import NodeID as NodeID
import Handle (Handle)
import Symbol (Symbol)
import File (File)

data Nature = Reference | Definition | Internal

derive instance eqNature :: Eq Nature
derive instance genNature :: Generic Nature _
instance Show Nature where show = genericShow

data Scoping = Scoped | Unscoped

derive instance eqScoping :: Eq Scoping
derive instance genScoping :: Generic Scoping _
instance Show Scoping where show = genericShow

data Visibility = Exported | Hidden

derive instance eqVisibility :: Eq Visibility
derive instance genVisibility :: Generic Visibility _
instance Show Visibility where show = genericShow

type DropScopesNode = { ident :: NodeID }

type SymbolNode = { ident :: NodeID, symbol :: Handle Symbol, scoping :: Scoping, nature :: Nature }

type ScopeNode = { ident :: NodeID, visibility :: Visibility }

data Node
  = Root
  | JumpTo
  | Push SymbolNode NodeID
  | Pop SymbolNode
  | Scope ScopeNode
  | Drop DropScopesNode

id :: Node -> NodeID
id n = case n of
  Root -> NodeID.root
  JumpTo -> NodeID.jumpTo
  Push s _ -> s.ident
  Pop s -> s.ident
  Scope s -> s.ident
  Drop s -> s.ident

isExportedScope :: Node -> Boolean
isExportedScope (Scope s) = s.visibility == Exported
isExportedScope _ = false

isDefinition :: Node -> Boolean
isDefinition (Push s _) = s.nature == Definition
isDefinition (Pop s) = s.nature == Definition
isDefinition _ = false

isReference :: Node -> Boolean
isReference (Push s _) = s.nature == Reference
isReference (Pop s) = s.nature == Reference
isReference _ = false

isJumpTo :: Node -> Boolean
isJumpTo JumpTo = true
isJumpTo _ = false

isRoot :: Node -> Boolean
isRoot Root = true
isRoot _ = false

symbol :: Node -> Maybe (Handle Symbol)
symbol (Push s _) = Just s.symbol
symbol (Pop s) = Just s.symbol
symbol _ = Nothing

scope :: Node -> Maybe NodeID
scope (Push _ n) = Just n
scope _ = Nothing

file :: Node -> Maybe (Handle File)
file n = (id n).file

isInFile :: Handle File -> Node -> Boolean
isInFile _ Root = true
isInFile _ JumpTo = true
isInFile f n = NodeID.isInFile f (id n)
