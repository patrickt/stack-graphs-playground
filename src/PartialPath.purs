module PartialPath where

import Data.Maybe (Maybe)
import Data.UInt (UInt)
import Handle (Handle)
import Node (Node)
import Path (ScopeStack, SymbolStack)

type Variable :: forall k. k -> Type
type Variable a = UInt

type PartialScopeStack = {
  scopes :: ScopeStack,
  variable :: Maybe (Variable ScopeStack)
}

type PartialScopedSymbol = {
  symbol :: Handle Symbol,
  scopes :: Maybe PartialScopeStack
}

type PartialSymbolStack = {
  symbols :: Array PartialScopedSymbol,
  variable :: Maybe (Variable SymbolStack)
}

type PartialPath = {
  startNode :: Handle Node,
  endNode :: Handle Node,
  symbolStackPrecondition :: PartialSymbolStack,
  scopeStackPrecondition :: PartialSymbolStack,
  symbolStackPostCondition :: PartialScopeStack
}
