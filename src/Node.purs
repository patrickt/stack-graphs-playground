module Node where

import Data.Hashable
import Data.Maybe
import Prelude
import Prim hiding (Symbol)

import Data.Function (on)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.UInt as UInt
import File (File)
import Handle (Handle)
import NodeID (NodeID)
import NodeID as NodeID
import Symbol (Symbol)

data Node
  = Root
  | JumpTo

  | PushSymbol { ident :: NodeID, symbol :: Handle Symbol, isReference :: Boolean }
  | PopSymbol { ident :: NodeID, symbol :: Handle Symbol, isDefinition :: Boolean }

  | PushScopedSymbol { ident :: NodeID, symbol :: Handle Symbol, scope :: NodeID, isReference :: Boolean }
  | PopScopedSymbol { ident :: NodeID, symbol :: Handle Symbol, isDefinition :: Boolean }

  | Scope { ident :: NodeID, isExported :: Boolean }
  | Drop { ident :: NodeID }

instance eqNode :: Eq Node where
  eq = eq `on` id

instance hashNode :: Hashable Node where
  hash n = UInt.toInt ((id n).localIdent)

id :: Node -> NodeID
id n = case n of
  Root -> NodeID.root
  JumpTo -> NodeID.jumpTo
  PushSymbol s -> s.ident
  PushScopedSymbol s -> s.ident
  PopScopedSymbol s -> s.ident
  PopSymbol s -> s.ident
  Scope s -> s.ident
  Drop s -> s.ident

isExportedScope :: Node -> Boolean
isExportedScope (Scope s) = s.isExported
isExportedScope _ = false

isDefinition :: Node -> Boolean
isDefinition (PopScopedSymbol s) = s.isDefinition
isDefinition (PopSymbol s) = s.isDefinition
isDefinition _ = false

isReference :: Node -> Boolean
isReference (PushScopedSymbol s) = s.isReference
isReference (PushSymbol s) = s.isReference
isReference _ = false

isJumpTo :: Node -> Boolean
isJumpTo JumpTo = true
isJumpTo _ = false

isRoot :: Node -> Boolean
isRoot Root = true
isRoot _ = false

symbol :: Node -> Maybe (Handle Symbol)
symbol (PushSymbol s) = Just s.symbol
symbol (PushScopedSymbol s) = Just s.symbol
symbol (PopSymbol s) = Just s.symbol
symbol (PopScopedSymbol s) = Just s.symbol
symbol _ = Nothing

scope :: Node -> Maybe NodeID
scope (PushScopedSymbol s) = Just s.scope
scope _ = Nothing

file :: Node -> Maybe (Handle File)
file n = (id n).file

isInFile :: Handle File -> Node -> Boolean
isInFile _ Root = true
isInFile _ JumpTo = true
isInFile f n = NodeID.isInFile f (id n)
